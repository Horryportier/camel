ANSITerminal.printf [ANSITerminal.magenta] "input cords or skip to use env valiables\n";
ANSITerminal.printf [ANSITerminal.green] "input latitude\n"
let lati = read_float_opt();;
ANSITerminal.printf [ANSITerminal.green] "input longitude\n"
let long = read_float_opt();;

Camel.Weather.curr lati long;;

