let fold_lefti f acc array =
  let len = Array.length array in
  let rec fold_tiles acc tiles i =
    if i < len then
      let acc = f acc i in 
      fold_tiles acc tiles (i+1)
    else acc
  in
  fold_tiles acc array 0