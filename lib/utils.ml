let float_of_option= function  None -> 0. | Some t -> t
let int_of_option = function None -> 0 | Some t -> t
let string_of_option = function None -> "" | Some t -> t


let get_env_cords =
    let lati = Sys.getenv_opt "LATITUDE" in
    let long = Sys.getenv_opt "LONGITUDE" in
    let lt = match lati with 
    | None -> Float.of_string  "0.0"
    | Some c -> Float.of_string  c in
    let lg = match long with
    | None -> Float.of_string  "0.0"
    | Some c -> Float.of_string  c in
    (lt, lg) 

let temp_to_style f = 
    let open ANSITerminal in
    match f with 
    | x when x < 10. -> [blue]
    | x when x < 15. -> [cyan]
    | x when x > 15. && x < 25. -> [green]
    | x when x > 25. && x < 30. -> [yellow]
    | x when x > 30. -> [red]
    | _ -> [black]
