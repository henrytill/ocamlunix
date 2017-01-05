open Unix

let get_fd filename =
  openfile filename [O_RDONLY] 0
