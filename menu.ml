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
;;

(*let print_menu () =
  p "==========================================================";
  p "[W] Enter Word Mode                                       ";
  p "[S] Enter Sentence Mode                                   ";
  p "[Q] "*)