library(rjson)
library(R6)
library(tidyverse)
library(processx)
# import subprocess
# import json
# from threading import Lock, Thread
# import io
# import os
# import sys
# import locale
# import platform

# try:
#     from json import JSONDecodeError
# except ImportError:
#     JSONDecodeError = ValueError


BaseScope <- R6Class(
    "BaseScope",
    public = list(
        # Tuple of class properties that will be passed as command-line
        # flags to configure scope
        scope_flags = c(),
        
        # Specify default chromium arguments
        default_chromium_args = c(
            "--disable-gpu",
            "--allow-file-access-from-files",
            "--disable-breakpad",
            "--disable-dev-shm-usage",
            # Add "--single-process" when running on AWS Lambda. Flag is described
            # as for debugging only by the chromium project, but it's the only way to get
            # chromium headless working on Lambda
            ifelse(Sys.info()['sysname'] == "LAMBDA_RUNTIME_DIR", c("--single-process"), c(""))
        ),
        
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
            return (append(cls._default_chromium_args , cls._scope_chromium_args))
        },
        initialize = function(
            disable_gpu = T,
            chromium_args = T,
            ...
        ){
            browser()
            if (chromium_args){
                chromium_args = self$default_chromium_args
            }else{ 
                if(!chromium_args)
                    chromium_args = c()
            }
            # Handle backward compatibility for disable_gpu flag
            if (!disable_gpu){
                # If disable_gpu is set to False, then remove corresponding flag from extra_chromium_args
                chromium_args = chromium_args[chromium_args != "--disable-gpu"]
            }
            self$chromium_args = list(chromium_args)
            
            # Internal Properties
            self$std_error = NULL #io.BytesIO()
            self$std_error_thread = NULL
            self$proc = NULL
            self$proc_lock = Lock()
        },
        del__ = function(){
            self$shutdown_kaleido()
        },
        
        executable_path = function(cls){
            vendored_executable_path = os$path$join(
                os$path$dirname(
                    os$path$dirname(
                        os$path$abspath(getwd())
                    )
                ),
                'executable',
                'kaleido'
            )
            
            # Add .cmd extension on Windows. The which function below doesn't need this, but os$path$exists requires
            # the file extension
            if (platform.system() == "Windows"){
                vendored_executable_path =  paste(vendored_executable_path, ".cmd")
            }
            if (os$path$exists(vendored_executable_path)){
                # The kaleido executable is vendored under kaleido/executable.
                # It was probably install as a PyPI wheel
                executable_path = vendored_executable_path
            }else{
                # The kaleido executable is not vendored under kaleido/executable,
                # Probably installed using conda, where the executable is a separate package
                # and is placed on the system PATH
                executable_path = which("kaleido")
                if (is.null(executable_path)){
                    path = Sys.getenv()['PATH']
                    formatted_path = path$replace(os$pathsep, "\n    ")
                    raiseValueError(
                        "
The kaleido executable is required by the kaleido Python library, but it was not included
in the Python package and it could not be found on the system path$

Searched for included kaleido executable at:
    {vendored_executable_path} 

Searched for executable 'kaleido' on the following system PATH:
    {formatted_path}\n" %>% format(
        vendored_executable_path=vendored_executable_path,
        formatted_path=formatted_path,
    )
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
        proc_args = list(self$executable_path(), self$scope_name)
        for (k in self$scope_flags){
            v = getattr(self, k)
            if (v) {
                flag = '--' + k.replace("_", "-")
            }else{
                if(!v | is.null(v) ){
                    # Logical flag set to False, don't include flag or argument
                    continue
                }else{
                    # Flag with associated value
                    flag = '--' + k.replace("_", "-") + "=" + repr(str(v))
                }
                proc_args %>% append(flag)
            }
        }
        # Append self$chromium_args
        proc_args %>% extend(self$chromium_args)
        
        return (proc_args)
    },
    collect_standard_error = function(){
        # """
        # Write standard-error of subprocess to the _std_error StringIO buffer.
        # Intended to be called once in a background thread
        # """
        # while True:
        # Usually there should aways be a process
        if (self$proc != None){
            val = self$proc$stderr.readline()
            self$std_error.write(val)
        }else{
            # Due to concurrency the process may be killed while this loop is still running
            # in this case break the loop
            return()
        }
    },
    ensure_kaleido = function(){
        # """
        # Launch the kaleido subprocess if it is not already running and in a good state
        # """
        # Use double-check locking to make sure we only initialize the process
        # from a single thread
        if (self$proc == None | self$proc$poll() != None)
            # with self$proc_lock:
            if (self$proc == None | self$proc$poll() != None)
                # Wait on process if crashed to prevent zombies
                if (self$proc != None){
                    self$proc$wait()
                }
        # Reset _std_error buffer
        self$std_error = io.BytesIO()
        
        # Launch kaleido subprocess
        # Note: shell=True seems to be needed on Windows to handle executable path with
        # spaces.  The subprocess.Popen docs makes it sound like this shouldn't be
        # necessary.
        
        ## Probably will have to use processx in R
        
        proc_args = self$build_proc_args()
        self$proc = process$new(
            proc_args,
            stdin = subprocess.PIPE,
            stdout = subprocess.PIPE,
            stderr = subprocess.PIPE,
            shell = sys.platform == "win32"
        )
        
        # Set up thread to asynchronously collect standard error stream
        if (self$std_error_thread == None | !self$std_error_thread.is_alive()){
            self$std_error_thread = Thread(target=self$collect_standard_error)
            self$std_error_thread.setDaemon(True)
            self$std_error_thread.start()
        }
        # Read startup message and check for errors
        startup_response_string = self$proc$stdout$readline()$decode('utf-8')
        if( !startup_response_string){
            message = (
                paste("Failed to start Kaleido subprocess. Error stream:\n\n", self$get_decoded_std_error())
            )
            raise (ValueError(message))
        }else{
            startup_response = json.loads(startup_response_string)
            if (startup_response.get("code", 0) != 0){
                self$proc$wait()
                raise (ValueError(startup_response.get("message", "Failed to start Kaleido subprocess")))
            }
        }
    },
    get_decoded_std_error = function(){
        # """
        # Attempt to decode standard error bytes stream to a string
        # """
        std_err_str = None
        try(
            {
                encoding = sys.stderr.encoding
                std_err_str = self$std_error.getvalue()$decode(encoding)
            },
            Exception = function(e){pass}
        )
        
        if (std_err_str == None){
            try({
                encoding = locale.getpreferredencoding(False)
                std_err_str = self$std_error$getvalue()$decode(encoding)
            },
            Exception = function(e){pass}
            )}
        
        if (std_err_str == None){
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
        if (self$proc != None){
            # with self$proc_lock:
            if (self$proc != None){
                if (self$proc$poll() == None){
                    # Process still running, close stdin to tell kaleido
                    # to shut down gracefully
                    self$proc$stdin$close()
                }
                # wait for process to terminate if it was running.
                # Also prevent zombie process if process crashed
                # on it's own
                try(
                    self$proc$wait(timeout=2.0),
                    except = function(x){
                        # We tried to wait! Moving on...
                        pass
                    })
                # Clear _proc property
                self$proc == None
            }
        }
        
        scope_name = function(){
            raise( NotImplementedError)
        }},
    # Flag property methods
    # is_disable_gpu = function(){
    #     # """ If True, asks chromium to disable GPU hardware acceleration with --disable-gpu flag"""
    #     return (self$chromium_args["--disable-gpu"])
    # },
    # @disable_gpu.setter
    disable_gpu = function(val){
        new_args = self$chromium_args[self$chromium_args != "--disable-gpu"]
        if (val){
            new_args.append("--disable-gpu")
        }
        self$chromium_args = tuple(new_args)
    },
    # #@property
    chromium_args = NULL,
    #@chromium_args.setter
    set_chromium_args = function(val){
        self$chromium_args = tuple(val)
        self$shutdown_kaleido()
    },
    json_dumps = function(val){
        return (json.dumps(val))
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
        export_spec = self$json_dumps(dict(..., data=data))$encode('utf-8')
        
        # Write to process and read result within a lock so that can be
        # sure we're reading the response to our request
        # with self$proc_lock:
        # Reset _std_error buffer
        self$std_error = io$BytesIO()
        
        # Write and flush spec
        self$proc$stdin$write(export_spec)
        self$proc$stdin$write("\n" %>% encode('utf-8'))
        self$proc$stdin$flush()
        response = self$proc$stdout$readline()
        
        response_string = response.decode('utf-8')
        if (!response_string){
            message = (
                "Transform failed. Error stream:\n\n" +
                    self$get_decoded_std_error()
            )
            raise (ValueError(message))}
        try(
            response = json.loads(response_string),
            except = function(x){ 
                # JSONDecodeError
                print(paste("Invalid JSON: ", repr(response_string)))
                raise()
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
        code = response.pop("code", 0)
        if (code != 0){
            message = response.get("message", None)
            raise (ValueError(
                "Transform failed with error code {code}: {message}" %>%
                    format(code=code, message=message)
            ))
        }
        
        img_string = response.pop("result", None)
        return( img_string.encode())
    })
)
# PATH helpers
which_py2 = function(cmd, mode=os.F_OK | os.X_OK, path=None){
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
    def _access_check(fn, mode):
        return os$path$exists(fn) and os.access(fn, mode) and not os$path$isdir(fn)
    
    # If we're given a path with a directory part, look it up directly rather
    # than referring to PATH directories. This includes checking relative to
    # the current directory, e.g. ./script
    if os$path$dirname(cmd):
        if _access_check(cmd, mode):
        return cmd
    return None
    
    if path is None:
        path = os$environ$get("PATH", os.defpath)
    if not path:
        return None
    path = path$split(os$pathsep)
    
    if sys.platform == "win32":
        # The current directory takes precedence on Windows.
        if not os.curdir in path:
        path$insert(0, os.curdir)
    
    # PATHEXT is necessary to check on Windows.
    pathext = os$environ$get("PATHEXT", "").split(os$pathsep)
    # See if the given file matches any of the expected path extensions.
    # This will allow us to short circuit when given "python.exe".
    # If it does match, only test that one, otherwise we have to try
    # others.
    if any(cmd.lower().endswith(ext.lower()) for ext in pathext):
        files = [cmd]
    else:
        files = [cmd + ext for ext in pathext]
    else:
        # On other platforms you don't have things like PATHEXT to tell you
        # what file suffixes are executable, so just pass on cmd as-is.
        files = [cmd]
    
    seen = set()
    for dir in path:
        normdir = os$path$normcase(dir)
    if not normdir in seen:
        seen.add(normdir)
    for thefile in files:
        name = os$path$join(dir, thefile)
    if _access_check(name, mode):
        return name
    return (None)
    
}
which = function(cmd){
    """
    Return the absolute path of the input executable string, based on the
    user's current PATH variable.

    This is a wrapper for shutil.which that is compatible with Python 2.

    Parameters
    ----------
    cmd: str
        String containing the name of an executable on the user's path$

    Returns
    -------
    str or None
        String containing the absolute path of the executable, or None if
        the executable was not found.

    """
    if sys.version_info > (3, 0):
        import shutil
    return (shutil.which(cmd))
    else:
        return (which_py2(cmd))
}