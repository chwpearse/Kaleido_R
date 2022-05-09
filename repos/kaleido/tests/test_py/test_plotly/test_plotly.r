# import os
# import sys
# from .. import baseline_root, tests_root
# from kaleido.scopes.plotly import PlotlyScope
# import pytest
# from .fixtures import all_figures, all_formats, mapbox_figure, simple_figure
# import plotly.graph_objects as go
# 
# import plotly.io as pio
# pio.templates.default = None

# if sys.version_info >= (3, 3):
#   from unittest.mock import Mock
# else:
#   from mock import Mock
library(assertthat)
library(mockr)

# os.environ['LIBGL_ALWAYS_SOFTWARE'] = '1'
# os.environ['GALLIUM_DRIVER'] = 'softpipe'

# Constants
mapbox_access_token <- Sys.getenv("MAPBOX_TOKEN")
local_plotlyjs_path <- file.path( getwd(),
                                 'repos',
                                 'kaleido',
                                 'tests',
                                 'test_py', "test_plotly", "resources", "plotly.min.js")
local_plotlyjs_url = local_plotlyjs_path %>% path.expand() #.as_uri()
baseline_root <- file.path(getwd(),
                           'repos',
                           'kaleido',
                           'tests',
                           'test_py', 'baselines')

mathjax = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js"

# Initialize a global scope, this way we test multiple uses
scope = PlotlyScope$new(
  mathjax=mathjax, mapbox_access_token=mapbox_access_token
)


load_baseline <- function(name, format){
  baseline_path = file.path(baseline_root, 'plotly', paste0(name, '.', format))
  f <- file(baseline_path, 'rb')
  expected = f %>% read_file_raw()#readBin('raw')
  close(f)
  return(expected)
}

write_baseline <- function(data, name, format){
  baseline_path = file.path(baseline_root, 'plotly', paste0(name, '.', format))
# with baseline_path.open('wb') as f:
  f <- file(baseline_path, 'rb')
  f %>% write_file(data)
  close(f)
}

write_failed <- function(data, name, format){
  failed_dir = file.path(baseline_root, 'plotly', "failed")
# failed_dir.mkdir(parents=True, exist_ok=True)
  dir.create(failed_dir, recursive = T)
  failed_path = file.path(failed_dir, paste0(name, '.', format))
  # with failed_path.open('wb') as f:
  #   f.write(data)
  f <- file(failed_path, 'wb')
  write_file(f, data)
}

#' @pytest.mark.parametrize('fig,name', all_figures())
#' @pytest.mark.parametrize('format', all_formats)
test_simple_figure <- function(fig, name, format){
  result = scope$transform(fig, format=format, width=700, height=500, scale=1)
  
  # Uncomment to create new baselines
  # write_baseline(result, name, format)
  
  expected = load_baseline(name, format)
  
  tryCatch(
    if (format == "svg"){
      # SVG not yet reproducible
      assert_that(result %>% str_starts('<svg')) # %>% charToRaw()))
    }else{
      if (format == "pdf"){
        # PDF not yet reproducible
        assert(result %>% str_starts('%PDF')) # %>% charToRaw()))
      }else{
        if (format == "emf"){
          # EMF not yet reproducible
          assert(result %>% str_starts("\\x01\\x00\\x00")) # %>% charToRaw()))
        }else{
          assert(result == expected)
        }
      }
    },
    error = function(e){
      stop(paste("Assertation failed: ", result, name, format))
    })
}

test_missing_mapbox_token <- function(){
  fig = mapbox_figure()
  local_scope = PlotlyScope$new(mapbox_access_token = NULL)
  # with pytest.raises(ValueError) as e:
  local_scope$transform(fig)

  assert_that(geterrmessage() %>% str_detect("access token"))
}

test_plotlyjs_file_url <- function(){
  fig = simple_figure()
  plotlyjs_url = local_plotlyjs_url
  local_scope = PlotlyScope$new(plotlyjs = plotlyjs_url %>% paste0("'", . , "'"))
  local_scope$ensure_kaleido %>% debugonce()
  result = local_scope$transform(fig, format='png', width=700, height=500, scale=1)
  expected = load_baseline('simple', 'png')
  assert_that(result == expected)
}

test_plotlyjs_local_file <- function(){
  fig = simple_figure()
  plotlyjs_path = local_plotlyjs_path
  local_scope = PlotlyScope$new(plotlyjs = plotlyjs_path)
  
  result = local_scope$transform(fig, format='png', width=700, height=500, scale=1)
  expected = load_baseline('simple', 'png')
  assert_that(result == expected)
}

test_plotlyjs_bad_local_file <- function(){
  plotlyjs_path = paste0(str(local_plotlyjs_path), ".bogus")
  # with pytest.raises(ValueError) as e:
  PlotlyScope$new(plotlyjs = plotlyjs_path)$transform(simple_figure())
  assert_that(geterrmessage() %>% str_detect("plotlyjs argument is not a valid URL"))
}

test_bad_format_file <- function(){
  fig = simple_figure()
  local_scope = PlotlyScope$new()
  # with pytest.raises(ValueError) as e:
  # tryCatch()
  local_scope$transform(fig, format='bogus')
  assert_that(geterrmessage() %>% str_detect("Invalid format"))
}

test_figure_size <- function(){
  # Create mocked scope
  # local_mock(transform_mock = function(...){list(code = 0, result = 'image')})
  
  PlotlyScope_mock <- R6Class(
    'PlotlyScope_mock',
    inherit = PlotlyScope,
    public = list(
      # perform_transform = transform_mock
      perform_transform = function(...){
        self$store_mock_call(...)
        self$increment_mock_calls()
        list(code = 0, result = 'image')
      },
      store_mock_call = function(...){
        # browser()
        private$mock_storage <- private$mock_storage %>% 
          append(list(list(...)))
      },
      increment_mock_calls = function(){
        private$mock_call_count <- private$mock_call_count + 1
      },
      get_mock_call_count = function(){
        return(private$mock_call_count)
      },
      get_mock_storage = function(n = NULL){
        if(!is.null(n)){
          return(private$mock_storage[[n]])
        }else{
          return(private$mock_storage)
        }
      },
      reset_mock = function(){
        private$mock_storage = list()
        private$mock_call_count = 0
      }
    ),
    private = list(
      mock_storage = list(),
      mock_call_count = 0
    )
  )
  
  scope = PlotlyScope_mock$new()
  # transform_mock = Mock(return_value = list(code = 0, result = image))
  # transform_mock = function(){return(list(code = 0, result = image))}
  # scope$perform_transform = transform_mock
  # local_mock(perform_transform = function(){}, clone = function(){}, .env = scope)
  
  # Set default width / height
  scope$default_width = 543
  scope$default_height = 567
  scope$default_format = "svg"
  scope$default_scale = 2
  
  # Make sure default width/height is used when no figure
  # width/height specified
  scope$reset_mock()
  fig = plot_ly()
  scope$transform(fig)
  assert_that(scope$get_mock_call_count() == 1)
  assert_that(
    map2_lgl(scope$get_mock_storage(1) %>% `[`(sort(names(.))), 
         list(fig, format="svg", scale=2, width=543, height=567) %>% `[`(sort(names(.))),
         identical) %>% all
    )
  
  # Make sure figure's width/height takes precedence over defaults
  scope$reset_mock()
  fig = plot_ly(width=123, height=234)
  scope$transform(fig)
  assert_that(scope$get_mock_call_count() == 1)
  assert_that(
    map2_lgl(scope$get_mock_storage(1) %>% `[`(sort(names(.))), 
             list(fig, format="svg", scale=2, width=123, height=234) %>% `[`(sort(names(.))),
             identical) %>% all
  )
  
  # Make sure kwargs take precedence over Figure layout values
  scope$reset_mock()
  fig = plot_ly(width=123, height=234)
  scope$transform(fig, width=987, height=876)
  assert_that(scope$get_mock_call_count() == 1)
  assert_that(
    map2_lgl(scope$get_mock_storage(1) %>% `[`(sort(names(.))), 
             list(fig, format="svg",  scale=2, width=987, height=876) %>% `[`(sort(names(.))),
             identical) %>% all
  )
}

test_gpu_arg <- function(){
  # --disable-gpu is a default
  assert_that( "--disable-gpu" %in% PlotlyScope$public_methods$default_chromium_args())
  
  # Check that --disable-gpu is in scope instance chromium_args
  scope = PlotlyScope$new()
  assert_that( "--disable-gpu" %in% scope$chromium_args)
  assert_that( "--disable-gpu" %in% scope$build_proc_args())
  
  # Check that --disable-gpu is in scope instance chromium_args
  scope = PlotlyScope$new(disable_gpu = FALSE)
  assert_that( "--disable-gpu" %in% scope$chromium_args %>% `!`)
  assert_that( "--disable-gpu" %in% scope$build_proc_args() %>% `!`)
  # debugonce(scope$disable_gpu)
  scope$disable_gpu(TRUE)
  assert_that( "--disable-gpu" %in% scope$chromium_args)
  assert_that( "--disable-gpu" %in% scope$build_proc_args())
}

test_custopm_chromium_arg <- function(){
  # Check that --disable-gpu is in scope instance chromium_args
  chromium_args = c(PlotlyScope$get_inherit()$public_fields$default_chromium_args, "--single-process")
  scope = PlotlyScope$new(chromium_args=chromium_args)
  assert_that( "--single-process" %in% scope$build_proc_args())
}

