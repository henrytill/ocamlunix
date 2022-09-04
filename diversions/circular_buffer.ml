type 'a t =
  { mutable read_pos : int
  ; mutable write_pos : int
  ; mutable element_count : int
  ; data : 'a array
  ; size : int
  }

let create size dummy =
  { read_pos = 0; write_pos = 0; element_count = 0; data = Array.make size dummy; size }
;;

let rec safe_add ceiling current amount =
  if current + amount >= ceiling
  then (
    let diff = current + amount - ceiling in
    safe_add ceiling 0 diff)
  else current + amount
;;

let safe_incr ceiling current = safe_add ceiling current 1

let push buffer elem =
  if buffer.element_count < buffer.size
  then buffer.element_count <- buffer.element_count + 1
  else buffer.read_pos <- safe_add buffer.size buffer.read_pos (buffer.element_count + 1);
  buffer.data.(buffer.write_pos) <- elem;
  buffer.write_pos <- safe_incr buffer.size buffer.write_pos
;;

let pop buffer =
  if buffer.element_count = 0 then raise Not_found;
  let result = buffer.data.(buffer.read_pos) in
  buffer.read_pos <- safe_incr buffer.size buffer.read_pos;
  buffer.element_count <- buffer.element_count - 1;
  result
;;
