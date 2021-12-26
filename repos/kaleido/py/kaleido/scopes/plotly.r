# from __future__ import absolute_import
# from kaleido.scopes.base import BaseScope, which
# import base64
# import os
# from pathlib import Path
# import subprocess

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
        
        initialize = function(self, plotlyjs=None, mathjax=None, topojson=None, mapbox_access_token=None, ...){
            # TODO: validate args
            # Save scope flags as internal properties
            self$plotlyjs = plotlyjs
            self$topojson = topojson
            self$mapbox_access_token = mapbox_access_token
            
            # Try to find local MathJax, but never fail if something goes wrong
            try:
                self$initialize_mathax(mathjax)
            except:
                self$mathjax = None
            
            # to_image-level default values
            self$default_format = "png"
            self$default_width = 700
            self$default_height = 500
            self$default_scale = 1
            
            super(PlotlyScope, self)$initialize(...)
        },
        initialize_mathax = function(self, mathjax=None){
            if (mathjax != None){
                self$mathjax = mathjax
                return()
            }
            
            vendored_mathjax_path = os$path$join(
                os$path$dirname(os$path$dirname(os$path$abspath(file__))),
                'executable',
                'etc',
                'mathjax',
                'MathJax.js'
            )
            mathjax_path = None
            if (os$path$exists(vendored_mathjax_path)){
                # MathJax is vendored under kaleido/executable.
                # It was probably install as a PyPI wheel
                mathjax_path = vendored_mathjax_path
            }else{
                mathjax_path_executable = which("mathjax-path")
            }
            if (mathjax_path_executable){
                # A script named "mathjax-path" found on the PATH,
                # MathJax was probably installed as a conda package
                path_bytes = subprocess$check_output(mathjax_path_executable)
                mathjax_path = path_bytes$decode("utf8")$strip()
            }
            if (mathjax_path){
                mathjax_uri = Path(mathjax_path)$absolute()$as_uri()
                self$mathjax = mathjax_uri
            }else{
                self$mathjax = None
            }
        },
        # @property
        scope_name = function(){
            return ("plotly")
        },
        json_dumps = function(val){
            # import plotly.io as pio
            return (pio$to_json(val, validate=False, remove_uids=False))
        },
        transform = function(self, figure, format=None, width=None, height=None, scale=None){
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
            if (isinstance(figure, Figure)){
                figure = figure$to_dict()
            }
            # Apply default format and scale
            format =  if (format != None){format} else {self$default_format}
            scale =  if (scale != None){scale} else{ self$default_scale}
            
            # Get figure layout
            layout = figure$get("layout", {})
            
            # Compute image width / height
            width = coalsce(
                width,
                layout$get("width", None),
                layout$get("template", c())$get("layout", c())$get("width", None),
                self$default_width
            )
            height = coalsce(
                height,
                layout$get("height", None),
                layout$get("template", c())$get("layout", c())$get("height", None),
                self$default_height
            )
            
            # Normalize format
            original_format = format
            format = format$lower()
            if (format == 'jpg'){
                format = 'jpeg'
            }
            if (!is_element(format, self$all_formats)){
                supported_formats_str = repr(list(self$all_formats))
                raise (
                    ValueError(
                        "Invalid format '{original_format}'.\nSupported formats: {supported_formats_str}" %>%
                            format(
                                original_format=original_format,
                                supported_formats_str=supported_formats_str
                            )
                    ))
            }
            # Transform in using _perform_transform rather than superclass so we can access the full
            # response dict, including error codes.
            response = self$perform_transform(
                figure, format=format, width=width, height=height, scale=scale
            )
            
            # Check for export error, later can customize error messages for plotly Python users
            code = response$get("code", 0)
            if (code != 0){
                message = response$get("message", None)
                raise (ValueError(
                    "Transform failed with error code {code}: {message}" %>% format(
                        code=code, message=message
                    )
                ))
            }
            img = response$get("result")$encode("utf-8")
            
            # Base64 decode binary types
            if (is_element(format, self$text_formats)){
                img = base64$b64decode(img)
            }
            return (img)
        },
        # Flag property methods
        #' property
        get_plotlyjs = function(self){
            #     """
            # URL or local file path to plotly.js bundle to use for image export.
            # If not specified, will default to CDN location.
            # """
            return (self$plotlyjs)
        },
        #' @plotlyjs.setter
        plotlyjs = function(self, val){
            self$plotlyjs = val
            self$shutdown_kaleido()
        },
        #' @property
        get_mathjax = function(self){
            # """
            # URL to MathJax bundle needed for LaTeX rendering.
            # If not specified, LaTeX rendering support will be disabled.
            # """
            return( self$mathjax)
        },
        #' @mathjax.setter
        mathjax = function(self, val){
            self$mathjax = val
            self$shutdown_kaleido()
        },
        #' @property
        get_topojson = function(){
            # """
            # URL to the topojson files needed to render choropleth traces.
            # If not specified, will default to CDN location.
            # """
            return (self$topojson)
        },
        #' @topojson.setter
        topojson = function(val){
            self$topojson = val
            self$shutdown_kaleido()
        },
        #' @property
        get_mapbox_access_token = function(){
            # """
            # Mapbox access token required to render mapbox layers.
            # If not specified, mapbox layers will only be rendered
            # if a valid token is specified inline in the figure specification
            # """
            return( self$mapbox_access_token)
        },
        #' @mapbox_access_token.setter
        mapbox_access_token = function(val){
            self$mapbox_access_token = val
            self$shutdown_kaleido()
        }
    )
)
