library(rjson)
library(R6)
# library(tidyverse)
library(processx)
library(plotly)

BaseScope <- R6Class(
    "BaseScope",
    public = list(
        # Tuple of class properties that will be passed as command-line
        # flags to configure scope
        scope_flags = c(),
        
        # Specify default chromium arguments
        default_chromium_args = c("--disable-gpu",
                                  "--allow-file-access-from-files",
                                  "--disable-breakpad",
                                  "--disable-dev-shm-usage") %>%
            append(if(Sys.info()[['sysname']] == "LAMBDA_RUNTIME_DIR"){c('word', 'b')}else{c()})
        # Add "--single-process" when running on AWS Lambda. Flag is described
        # as for debugging only by the chromium project, but it's the only way to get
        # chromium headless working on Lambda
        ,
        
        scope_chromium_args = c(),
        
        get_default_chromium_args = function(cls){
            # """
            # Get tuple containing the default chromium arguments that will be passed to chromium if not overridden.
            # 
            # chromium arguments can be overridden in the Scope constructor using the chromium_args argument, or they
            # can be overridden by assigning a tuple to the chromium_args property of an already constructed Scope instance
            # 
            # :return: tuple of str
            # """
            return (c(cls$default_chromium_args , cls$scope_chromium_args))
        },
        initialize = function(disable_gpu = T, chromium_args = T, ...){
            if (all(chromium_args == T)){
                chromium_args = self$default_chromium_args
            }else{ 
                if(all(chromium_args == F)){
                    chromium_args = c()
                }
            }
            # Handle backward compatibility for disable_gpu flag
            if (!disable_gpu){
                # If disable_gpu is set to False, then remove corresponding flag from extra_chromium_args
                chromium_args = chromium_args[chromium_args != "--disable-gpu"]
            }
            self$chromium_args = chromium_args
            
            # Internal Properties
            self$std_error_thread = NULL
            self$proc = NULL
            ## Process locking to allow multithreading not implemented
            # self$proc_lock = Lock()
            
        },
        # std_error = NULL,
        std_error_thread = NULL,
        proc = NULL,
        proc_lock = NULL,
        
        executable_path = function(cls){
            vendored_executable_path = file.path(
                getwd(),
                'kaleido_win_x64',
                'kaleido')
            
            # Add .cmd extension on Windows. The which function below doesn't need this, but os$path$exists requires
            # the file extension
            if (.Platform$OS.type == "windows"){
                vendored_executable_path =  paste0(vendored_executable_path, ".cmd")
            }
            if (file.exists(vendored_executable_path)){
                # The kaleido executable is vendored under kaleido/executable.
                # It was probably install as a PyPI wheel
                executable_path = vendored_executable_path
            }else{
                # The kaleido executable is not vendored under kaleido/executable,
                # Probably installed using conda, where the executable is a separate package
                # and is placed on the system PATH
                executable_path = Sys.which("kaleido")
                if (executable_path == ""){
                    path = Sys.getenv()['PATH']
                    formatted_path = gsub( ";", "\n    ", path)
                    stop(
                        paste(
                        "
The kaleido executable is required by the kaleido Python library, but it was not included
in the Python package and it could not be found on the system path$

Searched for included kaleido executable at:
    ", vendored_executable_path, " 

Searched for executable 'kaleido' on the following system PATH:
    ", formatted_path,"\n")
                    )
                }
            }
            return (executable_path)
        },
    build_proc_args = function(){
        # """
        # Build list of kaleido command-line arguments based on current values of
        # the properties specified by self$scope_flags and self$chromium_args
        # 
        # :return: list of flags
        # """
        proc_args = c(self$executable_path(), self$scope_name())
        for (k in self$scope_flags){
            v = self[[k]]()
            if(isTRUE(v)) {
                flag = paste0('--', gsub("_", "-", k))
            }else{
                if(isFALSE(v) | is.null(v) | anyNA(v)){
                    # Logical flag set to False, don't include flag or argument
                    next
                }else{
                    # Flag with associated value
                    flag = paste0('--', gsub("_", "-", k), "=", as.character(v))
                }
            }
            proc_args <- c(proc_args, flag)
        }
        # Append self$chromium_args
        proc_args <- c(proc_args, self$chromium_args)
        
        return (proc_args)
    },
    collect_standard_error = function(){
        # """
        # Write standard-error of subprocess to the _std_error StringIO buffer.
        # Intended to be called once in a background thread
        # """
        while(TRUE){
            # Usually there should always be a process
            if (!is.null(self$proc)){
                Sys.sleep(1)
                val = self$proc$read_error_lines()
                write(val, stderr())
            }else{
                # Due to concurrency the process may be killed while this loop is still running
                # in this case break the loop
                return()
            }
        }
    },
    ensure_kaleido = function(){
        # """
        # Launch the kaleido subprocess if it is not already running and in a good state
        # """
        # Use double-check locking to make sure we only initialize the process from a single thread
        if (is.null(self$proc) | !tryCatch({self$proc$is_alive()}, error = function(e){F})){
            # with self$proc_lock:
            if (is.null(self$proc) | !tryCatch({self$proc$is_alive()}, error = function(e){F})){
                # Wait on process if crashed to prevent zombies
                if (!is.null(self$proc)){
                    self$proc$wait()
                }
                # Reset _std_error buffer
                # self$std_error = io.BytesIO()
                
                # Launch kaleido subprocess
                # Note: shell=True seems to be needed on Windows to handle executable path with
                # spaces.  The subprocess.Popen docs makes it sound like this shouldn't be
                # necessary.
                
                ## Probably will have to use processx in R
                proc_args = self$build_proc_args()
                self$proc = processx::process$new(
                    command = proc_args[1], 
                    args = proc_args[-1],
                    stdin = "|",
                    stdout = "|",
                    stderr = "|"
                )
                
                # Set up thread to asynchronously collect standard error stream
                if (is.null(self$std_error_thread) | tryCatch({self$std_error_thread %>% is_alive() %>% `!`}, error = function(e){F})){
                    self$std_error_thread = self$proc$get_error_connection()
                }
                # Read startup message and check for errors
                for(i in 0:6){
                    # If there is no output because process is still starting up retry every .5 seconds for 3 seconds
                    startup_response_string = self$proc$read_output_lines()
                    # browser()
                    if(length(startup_response_string) > 0){break}
                    Sys.sleep(.5)
                }
                if(length(startup_response_string) == 0){
                    message = paste("Failed to start Kaleido subprocess. Error stream:\n\n", self$get_decoded_std_error())
                    stop(message)
                }else{
                    startup_response = rjson::fromJSON(startup_response_string)
                    if (startup_response[["code"]][[1]] != 0){
                        self$proc$wait()
                        stop(paste(startup_response$message,  "Failed to start Kaleido subprocess"))
                    }
                }
            }
        }
    },
    get_decoded_std_error = function(){
        # """
        # Attempt to decode standard error bytes stream to a string
        # """
        std_err_str = NA
        tryCatch(
            {
                std_err_str <- self$proc$read_error()
            },
            error = function(e){NA}
        )
        
        if (is.na(std_err_str)){
            tryCatch({
                std_err_str <- self$proc$read_error()
            },
            error = function(e){NA}
            )}
        
        if (is.na(std_err_str)){
            std_err_str = "Failed to decode Chromium's standard error stream"
        }
        return (std_err_str)
    },
    shutdown_kaleido = function(){
        # """
        # Shut down the kaleido subprocess, if any, and self the _proc property to None
        # """
        # Use double-check locking to make sure we only shut down the process
        # a single time when used across threads.
        res <- NULL
        if (!is.null(self$proc)){
            # with self$proc_lock:
            if (!is.null(self$proc)){
                if(self$proc$is_alive()){ #is.na(self$proc$poll_io(1))){
                    # Process still running, close stdin to tell kaleido
                    # to shut down gracefully
                    res <- self$proc$kill()
                }
                # wait for process to terminate if it was running.
                # Also prevent zombie process if process crashed
                # on it's own
                tryCatch(
                    self$proc$wait(timeout = 1000),
                    error = function(x){
                        # We tried to wait! Moving on...
                        NA
                    })
                # NULL proc property
                self$proc = NULL
            }
        }
    },
    
    scope_name = function(){
        stop( 'NotImplementedError')
    },
    # Flag property methods
    # is_disable_gpu = function(){
    #     # """ If True, asks chromium to disable GPU hardware acceleration with --disable-gpu flag"""
    #     return (self$chromium_args["--disable-gpu"])
    # },
    # @disable_gpu.setter
    disable_gpu = function(val){
        new_args = self$chromium_args[self$chromium_args != "--disable-gpu"]
        if (val){
            new_args <- c(new_args, "--disable-gpu")
        }
        self$chromium_args = new_args
    },
    # #@property
    chromium_args = NULL,
    #@chromium_args.setter
    set_chromium_args = function(val){
        self$chromium_args = list(val)
        self$shutdown_kaleido()
    },
    json_dumps = function(val){
        return(jsonlite::toJSON(val))
        # return(val)
    },
    perform_transform = function(data, ...){
        # """
        # Transform input data using the current scope, returning dict response with error code
        # whether successful or not.
        # 
        # :param data: JSON-serializable object to be converted
        # :param kwargs: Transform arguments for scope
        # :return: Dict of response from Kaleido executable, whether successful or not
        # """
        # Ensure that kaleido subprocess is running
        self$ensure_kaleido()
        
        # Perform export
        # browser()
        export_spec = self$json_dumps(list(..., data=data)) #$encode('utf-8')
        
        # Write to process and read result within a lock so that can be
        # sure we're reading the response to our request
        # with self$proc_lock:
        # Reset _std_error buffer
        # self$std_error = io$BytesIO()
        
        # Write and flush spec
        self$proc$write_input(export_spec)
        self$proc$write_input("\n") # %>% `Encoding<-`('utf-8'))
        # self$proc$stdin$flush()
        # response = self$proc$get_output_connection() %>% processx_conn_read_lines()
        
        response = self$proc$read_output_lines(1)
        
        # while(response == ""){
        while(length(response) < 1){
        # while(response %>% str_detect("\\}", negate = T)){
            # if no response yet wait and try again
            Sys.sleep(.1)
            response = self$proc$read_output_lines()
        }
        
        response_string = response #%>% fromJSON() %>% paste(names(.), ":", ., collapse = '\n') #.decode('utf-8')
        
        if (is.null(response_string)){
            message = paste0(
                "Transform failed. Error stream:\n\n",
                    self$get_decoded_std_error()
            )
            stop(message)
        }
        
        response <- tryCatch(
            fromJSON(response_string),
            error = function(x){ 
                # JSONDecodeError
                # browser()
                response_string <- paste0("Invalid JSON: ", response_string) #repr(response_string)))
                stop(response_string)
            }
        )
        return (response)
    },
    transform = function ( data, ...){
        # """
        # Transform input data using the current scope
        # 
        # Subclasses should provide a more helpful docstring
        # 
        # :param data: JSON-serializable object to be converted
        # :param kwargs: Transform arguments for scope
        # :return: Transformed value as bytes
        # """
        response = self$perform_transform(data, ...)
        
        # Check for export error
        code = response$code[[1]]
        if (code != 0){
            message = response$message
            stop(paste0("Transform failed with error code ", code, ": ", message))
        }
        
        img_string = response %>% chuck("result", NA)
        return(img_string %>% encode())
    }),
    private = list(
        finalize = function() {
            self$shutdown_kaleido()
        }
    )
)

# PATH helpers
which_py2 = function(cmd, mode = 1, path = NA){
    # """
    # Backport (unmodified) of shutil.which command from Python 3.6
    # Remove this when Python 2 support is dropped
    # 
    # Given a command, mode, and a PATH string, return the path which
    # conforms to the given mode on the PATH, or None if there is no such
    # file.
    # 
    # `mode` defaults to os.F_OK | os.X_OK. `path` defaults to the result
    # of os.environ.get("PATH"), or can be overridden with a custom search
    # path$
    # """
    # Check that a given file can be accessed with the correct mode.
    # Additionally check that `file` is not a directory, as on Windows
    # directories pass the os.access check.
    access_check <- function(fn, mode){
        return (dir.exists(fn) & file.access(fn, mode) &  !os$path$isdir(fn))
    }
    # If we're given a path with a directory part, look it up directly rather
    # than referring to PATH directories. This includes checking relative to
    # the current directory, e.g. ./script
    if (dirname(cmd)){
        if (access_check(cmd, mode)){
            return(cmd)
        }
    }else{
        return(NA)
    }
    if (is.na(path)){
        path = os$environ$get("PATH", os.defpath)
    }
    if  (!path){
        return( NA)
    }
    path = path$split(os$pathsep)
    
    if (.Platform$OS.type == "windows"){
        # The current directory takes precedence on Windows.
        if (!(os.curdir %in% path)){
            path$insert(0, os.curdir)
        }
        
        # PATHEXT is necessary to check on Windows.
        pathext = os$environ$get("PATHEXT", "") %>% split(os$pathsep)
        # See if the given file matches any of the expected path extensions.
        # This will allow us to short circuit when given "python.exe".
        # If it does match, only test that one, otherwise we have to try
        # others.
        if( any(lapply(pathext, FUN = function(ext){endswith(lower(cmd), lower(ext))}))){
            files = cmd
        }else{
            files = paste0(cmd, pathext)
        }
    }else{
        # On other platforms you don't have things like PATHEXT to tell you
        # what file suffixes are executable, so just pass on cmd as-is.
        files = cmd
    }
    seen = set()
    for (dir in path){
        normdir = os$path$normcase(dir)
        if (!( normdir %in% seen)){
            seen.add(normdir)
            for (thefile in files){
                name = os$path$join(dir, thefile)
                if (access_check(name, mode))
                    return (name)}}}
    return (None)
}

kaleido_which = function(cmd){
    # """
    # Return the absolute path of the input executable string, based on the
    # user's current PATH variable.
    # 
    # This is a wrapper for shutil.which that is compatible with Python 2.
    # 
    # Parameters
    # ----------
    # cmd: str
    #     String containing the name of an executable on the user's path$
    # 
    # Returns
    # -------
    # str or None
    #     String containing the absolute path of the executable, or None if
    #     the executable was not found.
    # 
    # """
    if (sys.version_info > c(3, 0)){
        # import (shutil)
        return (shutil_which(cmd))
    }else{
        return (which_py2(cmd))
    }
}

