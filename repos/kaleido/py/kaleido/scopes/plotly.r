# from __future__ import absolute_import
# from kaleido.scopes.base import BaseScope, which
# import base64
# import os
# from pathlib import Path
# import subprocess

library(R6)

PlotlyScope <- R6Class(
    classname = "PlotlyScope",
    inherit = BaseScope,
    # """
    # Scope for transforming Plotly figures to static images
    # """
    public = list(
        all_formats = c("png", "jpg", "jpeg", "webp", "svg", "pdf", "eps", "json"),
        text_formats = c("svg", "json", "eps"),
        
        scope_flags = c("plotlyjs", "mathjax", "topojson", "mapbox_access_token"),
        scope_chromium_args = c("--no-sandbox"),
        
        initialize = function(plotlyjs = NA, mathjax = NA, topojson = NA, mapbox_access_token = NA, ...){
            # TODO: validate args
            # Save scope flags as internal properties
            self$set_plotlyjs(plotlyjs)
            self$set_topojson(topojson)
            self$set_mapbox_access_token(mapbox_access_token)
            
            # Try to find local MathJax, but never fail if something goes wrong
            # try:
            tryCatch(
                self$initialize_mathax(mathjax),
                # except:
                error = function(e){self$set_mathjax(NA)})
            
            # to_image-level default values
            self$default_format = "png"
            self$default_width = 700
            self$default_height = 500
            self$default_scale = 1
            
            super$initialize(...)
        },
        initialize_mathax = function(mathjax= NA){
            if (!is.na(mathjax)){
                self$set_mathjax(mathjax)
                return()
            }
            
            vendored_mathjax_path = file.path(
                dirname(dirname(path.expand(getwd()))),
                'Kaleido',
                'Kaleido R',
                'kaleido_win_x64',
                'etc',
                'mathjax',
                'MathJax.js'
            )
            mathjax_path = NA
            if (file.exists(vendored_mathjax_path)){
                # MathJax is vendored under kaleido/executable.
                # It was probably install as a PyPI wheel
                mathjax_path = vendored_mathjax_path
            }else{
                mathjax_path_executable = which("mathjax-path")
                
                if (!is.na(mathjax_path_executable)){
                    # A script named "mathjax-path" found on the PATH,
                    # MathJax was probably installed as a conda package
                    path_bytes = subprocess$check_output(mathjax_path_executable)
                    mathjax_path = path_bytes$decode("utf8")$strip()
                }
            }
            if (!is.na(mathjax_path)){
                mathjax_uri = mathjax_path %>% paste0("'", . , "'")#Path(mathjax_path)$absolute()$as_uri()
                self$set_mathjax(mathjax_uri)
            }else{
                self$set_mathjax(NA)
            }
        },
        # @property
        scope_name = function(){
            return ("plotly")
        },
        json_dumps = function(val){
            # import plotly.io as pio
            # return (plotly:::to_JSON(val, validate=False, remove_uids=False))
            
            plotly_transformed <- plotly:::to_JSON(val, validate=False, remove_uids=False)
            # plotly_transformed <- plotly_json(built_plotly) #, validate=False, remove_uids=False)
            return(plotly_transformed)
        },
        transform = function(figure, format= NULL, width = NULL, height= NULL, scale= NULL){
            # """
            # Convert a Plotly figure into a static image
            # 
            # :param figure: Plotly figure or figure dictionary
            # :param format: The desired image format. One of
            #    'png', 'jpg', 'jpeg', 'webp', 'svg', 'pdf', or 'json'.
            # 
            #    If 'json', the following arguments are ignored and a full
            #    JSON representation of the figure is returned.
            # 
            #    If not specified, will default to the `scope.default_format` property
            # :param width: The width of the exported image in layout pixels.
            #     If the `scale` property is 1.0, this will also be the width
            #     of the exported image in physical pixels.
            # 
            #     If not specified, will default to the `scope.default_width` property
            # :param height: The height of the exported image in layout pixels.
            #     If the `scale` property is 1.0, this will also be the height
            #     of the exported image in physical pixels.
            # 
            #     If not specified, will default to the `scope.default_height` property
            # :param scale: The scale factor to use when exporting the figure.
            #     A scale factor larger than 1.0 will increase the image resolution
            #     with respect to the figure's layout pixel dimensions. Whereas as
            #     scale factor of less than 1.0 will decrease the image resolution.
            # 
            #     If not specified, will default to the `scope.default_scale` property
            # :return: image bytes
            # """
            # TODO: validate args
            
            # from plotly.graph_objects import Figure
            # if (class(figure)[[1]] == 'Plotly'){
            #     # figure = figure$to_dict()
            #     figure = figure %>% plotly_build
            # }
            # Apply default format and scale
            format =  if (!is.null(format)){format} else {self$default_format}
            scale =  if (!is.null(scale)){scale} else{ self$default_scale}
            
            # Get figure layout
            layout = figure$x$layout
            
            # Compute image width / height
            width = c(
                width,
                layout$width,
                layout$template$layout$width,
                self$default_width
            )[[1]]
            height = c(
                height,
                layout$height,
                layout$template$layout$height,
                self$default_height
            )[[1]]
            
            # Normalize format
            original_format = format
            format = format %>% str_to_lower()
            if (format == 'jpg'){
                format = 'jpeg'
            }
            if (!is.element(format, self$all_formats)){
                supported_formats_str = self$all_formats %>% paste(collapse = ', ')
                stop (
                    paste0(
                        "Invalid format '", original_format, "'.\nSupported formats: ", supported_formats_str
                    ))
            }
            # Transform in using _perform_transform rather than superclass so we can access the full
            # response dict, including error codes.
            # browser()
            built_plotly <- plotly_build(figure)$x[c("data", "layout", "config")]
            response = self$perform_transform(
                # figure$x, format=format, width=width, height=height, scale=scale
                # figure, format=format, width=width, height=height, scale=scale
                built_plotly, format=format, width=width, height=height, scale=scale
            )
            
            # Check for export error, later can customize error messages for plotly Python users
            code = response$code
            if (code != 0){
                message = response$message
                stop (
                    paste0("Transform failed with error code ", code, ": ", message)
                )
            }
            img = response$result #$get("result")$encode("utf-8")
            
            # Base64 decode binary types
            if (!is.element(format, self$text_formats)){
                img = base64enc::base64decode(img)
            }
            return (img)
        },
        # Flag property methods
        #' property
        #     """
        # URL or local file path to plotly.js bundle to use for image export.
        # If not specified, will default to CDN location.
        # """
        plotlyjs = function(){
            return(private$plotlyjs_)
        },
        #' @plotlyjs.setter
        set_plotlyjs = function(val){
            private$plotlyjs_ = val
            self$shutdown_kaleido()
        },
        #' @property
        # """
        # URL to MathJax bundle needed for LaTeX rendering.
        # If not specified, LaTeX rendering support will be disabled.
        # """
        mathjax = function(){
            return(NA)
            return(private$mathjax_)
        },
        set_mathjax = function(val){
            private$mathjax_ = val
            self$shutdown_kaleido()
        },
        #' @property
        # """
        # URL to the topojson files needed to render choropleth traces.
        # If not specified, will default to CDN location.
        # """
        topojson = function(){
            return(self$topojson_)
        },
        #' @topojson.setter
        set_topojson = function(val){
            private$topojson_ = val
            self$shutdown_kaleido()
        },
        #' @property
        # """
        # Mapbox access token required to render mapbox layers.
        # If not specified, mapbox layers will only be rendered
        # if a valid token is specified inline in the figure specification
        # """
        mapbox_access_token = function(){
            return(private$mapbox_access_token_)
        },
        #' @mapbox_access_token.setter
        set_mapbox_access_token = function(val){
            private$mapbox_access_token_ = val
            self$shutdown_kaleido()
        },
        default_format = NULL,
        default_width = NULL,
        default_height = NULL,
        default_scale = NULL
    ),
    private = list(
        plotlyjs_ = NA,
        mathjax_ = NA,
        topojson_ = NA,
        mapbox_access_token_ = NA
    )
)

