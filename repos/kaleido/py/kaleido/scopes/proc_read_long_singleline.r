proc_read_long_singleline <- function(my_proc, wait = 100, timeout = 5000){
  my_proc$poll_io(wait)
  counter <- 1
  output_lines_res <- my_proc$read_output_lines(1)
  output_res <- c()
  
  while(length(output_lines_res) == 0 & counter*wait <= timeout){
    output_res <- append(output_res, my_proc$read_output())
    my_proc$poll_io(wait)
    counter <- counter + 1
    output_lines_res <- append(output_lines_res, my_proc$read_output_lines(1))
  }
  
  res_output <- paste0(paste0(output_res, collapse = ''), output_lines_res)
  return(res_output)
}