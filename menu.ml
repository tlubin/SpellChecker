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
  p ("Would you like to enter word mode (W), sentence mode (S), file mode (F), " ^
     "help (H), or quit (Q)? : ")
