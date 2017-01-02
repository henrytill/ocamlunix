exception Found

let for_all (pred: 'a -> bool) (arr: 'a array): bool =
  try
    for i = 0 to Array.length arr - 1 do
      if not (pred arr.(i)) then raise Found
    done;
    true
  with
    Found -> false

include Array
