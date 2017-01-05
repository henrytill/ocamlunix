(* Inspired by: https://groups.google.com/d/msg/fa.caml/GP9p9wTo-Cs/NVTl3idPSLYJ *)
type 'a t =
  { mutable read_pos : int;
    mutable write_pos : int;
    mutable element_count : int;
    data : 'a array;
    size : int }

let create size dummy =
  { read_pos = 0;
    write_pos = 0;
    element_count = 0;
    data = Array.make size dummy;
    size }

let safe_incr ceiling current =
  if current = ceiling - 1 then 0 else current + 1

let push buffer elem =
  if buffer.element_count < buffer.size then buffer.element_count <- buffer.element_count + 1;
  buffer.data.(buffer.write_pos) <- elem;
  let read_write_pos = safe_incr buffer.size buffer.write_pos in
  buffer.write_pos <- read_write_pos;
  buffer.read_pos <- read_write_pos

let pop buffer =
  if buffer.element_count = 0 then raise Not_found;
  let result = buffer.data.(buffer.read_pos) in
  buffer.read_pos <- safe_incr buffer.size buffer.read_pos;
  buffer.element_count <- buffer.element_count - 1;
  result
