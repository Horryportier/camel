open Lwt
(* open Cohttp *)
open Cohttp_lwt_unix



type current_weather = {
    time: string option;
    temperature: float option;
    wind_speed: float option;
    wind_direection: float option;
}

     

let make_url lati long =
   Printf.sprintf "https://api.open-meteo.com/v1/forecast?latitude=%.2f&longitude=%.2f&current_weather=true" lati long

let call_api (lati, long) = 
    let url = make_url lati long in
    let body url =
        Client.get (Uri.of_string url) >>= fun (_, body) ->
        body |> Cohttp_lwt.Body.to_string >|= fun body ->
        body in
    body url



let parse_api_call json = 
    let json = Yojson.Basic.from_string json in
    json
    


let to_current_weather json =
    let open Yojson.Basic.Util in
         let current_weather = json |> member "current_weather" in
         let temperature = current_weather |> member "temperature" |> to_float_option in
         let wind_speed = current_weather |> member "windspeed" |> to_float_option in
         let wind_direection = current_weather |> member "winddireection" |> to_float_option in
         let time = current_weather |> member "time" |> to_string_option in
         {time; temperature; wind_speed; wind_direection};;




let print_curr c=
    let open ANSITerminal in
    let open Utils in
    let temp = c.temperature |> float_of_option in
    Printf.printf "temp: %s  time: %s  ws: %s wd: %s" 
    (sprintf (temp_to_style temp) "%.2f" temp ) 
    (sprintf [black] "%s" (c.time |> string_of_option))
    (sprintf [cyan] "%.2f" (c.wind_speed |> float_of_option))
    (sprintf [yellow] "%.2f" (c.wind_direection|> float_of_option))

    
let cords lati long = 
    let env = Utils.get_env_cords in
    let (is_lati, lati) = match lati with
    | Some c -> (true, c)
    | None -> (false, 0.0) in
    let (is_long, long) = match long with
    | Some c -> (true, c)
    | None -> (false, 0.0) in
    if is_lati && is_long then (lati, long) else env

let curr lati long = 
    let resp = call_api (cords lati long) in
    let body = Lwt_main.run resp in
    let json = parse_api_call body in
    let curr = to_current_weather json in
    print_curr curr;;
