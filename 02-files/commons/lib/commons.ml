open Unix

exception End_of_file

type in_channel =
  { in_buffer: bytes;
    in_fd: file_descr;
    mutable in_pos: int;
    mutable in_end: int }

let buffer_size = 8192

let open_in filename =
  { in_buffer = Bytes.create buffer_size;
    in_fd = openfile filename [O_RDONLY] 0;
    in_pos = 0;
    in_end = 0 }

let input_char chan =
  if chan.in_pos < chan.in_end then
    begin
      let c = Bytes.get chan.in_buffer chan.in_pos in
      chan.in_pos <- chan.in_pos + 1;
      c
    end
  else
    begin
      match read chan.in_fd chan.in_buffer 0 buffer_size with
      | 0 -> raise End_of_file
      | r ->
        chan.in_end <- r;
        chan.in_pos <- 1;
        Bytes.get chan.in_buffer 0;
    end

let close_in chan =
  close chan.in_fd

type out_channel =
  { out_buffer: bytes;
    out_fd: file_descr;
    mutable out_pos: int }

let open_out filename =
  { out_buffer = Bytes.create buffer_size;
    out_fd = openfile filename [O_WRONLY; O_TRUNC; O_CREAT] 0o666;
    out_pos = 0 }

let output_char chan c =
  if chan.out_pos < Bytes.length chan.out_buffer then
    begin
      Bytes.set chan.out_buffer chan.out_pos c;
      chan.out_pos <- chan.out_pos + 1
    end
  else
    begin
      ignore (write chan.out_fd chan.out_buffer 0 chan.out_pos);
      Bytes.set chan.out_buffer 0 c;
      chan.out_pos <- 1
    end

let output_string chan s =
  let avail = Bytes.length chan.out_buffer - chan.out_pos in
  let len = String.length s in
  if len <= avail then
    begin
      Bytes.blit_string s 0 chan.out_buffer chan.out_pos len;
      chan.out_pos <- chan.out_pos + len
    end
  else if chan.out_pos = 0 then
    begin
      ignore (write chan.out_fd (Bytes.of_string s) 0 len)
    end
  else
    begin
      Bytes.blit_string s 0 chan.out_buffer chan.out_pos avail;
      let out_buffer_size = Bytes.length chan.out_buffer in
      ignore (write chan.out_fd chan.out_buffer 0 out_buffer_size);
      let remaining = len - avail in
      if remaining < out_buffer_size then
        begin
          Bytes.blit_string s avail chan.out_buffer 0 remaining;
          chan.out_pos <- remaining
        end
      else
        begin
          ignore (write chan.out_fd (Bytes.of_string s) avail remaining);
          chan.out_pos <- 0
        end
    end

let close_out chan =
  ignore (write chan.out_fd chan.out_buffer 0 chan.out_pos);
  close chan.out_fd
