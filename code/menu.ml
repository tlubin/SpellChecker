let p str = Printf.printf "%s\n" str

let print_header () =
  p "                                                                                                                         ";
  p "    _|_|_|                                                      _|                ";
  p "  _|          _|_|    _|  _|_|  _|  _|_|    _|_|      _|_|_|  _|_|_|_|  _|  _|_|  ";
  p "  _|        _|    _|  _|_|      _|_|      _|_|_|_|  _|          _|      _|_|      ";
  p "  _|        _|    _|  _|        _|        _|        _|          _|      _|        ";
  p "    _|_|_|    _|_|    _|        _|          _|_|_|    _|_|_|      _|_|  _|        ";
  p "                                                                                                                         ";
  ()


let print_mode_prompt () =
  print_string ("Would you like to enter word mode (W), sentence mode (S), file mode (F), " ^
     "help (H), or quit (Q)? : ")

let print_spaces n =
  if n <= 0 then print_string "     "
  else 
    print_string (String.make n ' ')
