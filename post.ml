let altitude pix = 
  let rec alt pi li = match li with
  | [] -> 0
  | e::li when e.Refe.rgb = pi -> e.Refe.alt
  | e::_ -> alt pi li
  in alt pix (Refe.get_list_alt())


