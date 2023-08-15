let float_of_option= function  None -> 0. | Some t -> t
let int_of_option = function None -> 0 | Some t -> t
let string_of_option = function None -> "" | Some t -> t

let to_styled modifier color input_str = 
        Printf.sprintf " \\u001b[%i;%im%s\\u001b[0m" modifier color input_str

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
