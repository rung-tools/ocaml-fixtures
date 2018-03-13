(**
 * MIT License
 * 
 * Copyright (c) 2018 NG Informática
 *
 * Written by:
 *
 * - Marcelo Camargo <undefined.void@null.net>
 * - Paulo Torrens <paulotorrens@gnu.org>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **)
open Core_extended

type file_system =
    | Dir of string * file_system list
    | File of string * string
    [@@deriving show]

type test = {
    flags: string;
    description: string;
    input: string;
    output: string;
}
[@@deriving show]

type parser_section =
    | Epsilon
    | Flags
    | Description
    | Input
    | Output

let get_test_info path =
    Core.In_channel.read_lines path
    |> List.fold_left
        (fun (prev_state, section) line ->
            match line with
            | "[FLAGS]"       -> (prev_state, Flags)
            | "[DESCRIPTION]" -> (prev_state, Description)
            | "[INPUT]"       -> (prev_state, Input)
            | "[OUTPUT]"      -> (prev_state, Output)
            | line ->
                match section with
                | Flags -> ({ prev_state with flags = prev_state.flags ^ line }, section)
                | Description -> ({ prev_state with description = prev_state.description ^ line }, section)
                | Input -> ({ prev_state with input = prev_state.input ^ line ^ "\n" }, section)
                | Output -> ({ prev_state with output = prev_state.output ^ line ^ "\n" }, section)
                | Epsilon -> (prev_state, section)
        ) ({ flags = ""; description = ""; input = ""; output = "" }, Epsilon)
    |> fst

let read_dir_test_files root =
    let rec loop name path =
        match Sys.is_directory path with
        | true  ->
            let children = Sys.readdir path
            |> Array.to_list
            |> List.map (fun name -> loop name (Filename.concat path name))
            |> List.filter (function
                | Dir (_, [])    -> false
                | Dir _          -> true
                | File (name, _) -> Filename.check_suffix name ".mlt") in
            Dir (name, children)
        | false -> File (name, path)
    in loop root root

let gen_fortune_cookie () =
    let niilist_messages = [|
        "A vida é um pato que se come frio";
        "Será que estamos vivendo ou apenas existindo?";
        "Não separa-se predicado por vírgula";
        "Sai do meu grupo";
        "Os testes passaram, mas minha vontade de morrer permanece";
        "Você deveria cobrar insalubridade por trabalhar com Clipper"
    |] in
    Random.self_init ();
    let index = Random.int (Array.length niilist_messages) in
    niilist_messages.(index)

let rec tree_size node =
    match node with
    | Dir (_, children) ->
        children
        |> List.fold_left (fun size node -> size + (tree_size node)) 0
    | File _ -> 1

let run_tests tree =
    let test_passed_symbol = Color_print.green "[ ✓ ]" in
    let test_failed_symbol = Color_print.red "[ ✗ ]" in
    let indent level = String.make (4 * level) ' ' in
    let rec run node ?(level = 1) () =
        match node with
        | Dir ("./", children) ->
            let target_directory = Sys.getcwd ()
            |> Color_print.underline
            |> Color_print.gray ~brightness:1.0 in
            Color_print.blue_printf "\n    Running on directory [%s] (%d tests)\n\n"
                target_directory (tree_size tree);
            List.iter (fun node -> run node ~level:(level + 1) ()) children
        | Dir (name, children) ->
            Printf.printf "%s - %s/\n" (indent level) (Color_print.blue name);
            List.iter (fun node -> run node ~level:(level + 1) ()) children
        | File (name, path) ->
            let test_info = get_test_info path in
            let (temp_name, temp_channel) = Filename.open_temp_file "test" ".txt" in
            Core.Out_channel.output_string temp_channel test_info.input;
            Core.Out_channel.close temp_channel;
            let start_time = Sys.time () in
            let command = Printf.sprintf "cat %s | ../_build/default/bin/main.exe %s" temp_name test_info.flags in
            let in_channel = Unix.open_process_in command in
            let execution_result = Core.In_channel.input_all in_channel in
            let success = (String.trim execution_result) = (String.trim test_info.output) in
            let symbol = if success then test_passed_symbol else test_failed_symbol in
            let finish_time = Sys.time () in
            let total_millis = (finish_time -. start_time) *. 1000.0 in
            Printf.printf "%s%s - %s %s\n"
                (indent level) symbol test_info.description
                (Color_print.yellow_sprintf "(%.3fms)" total_millis);
            if not success then begin
                let (expected_name, expected_channel) = Filename.open_temp_file "expected" ".txt" in
                Core.Out_channel.output_string expected_channel test_info.output;
                Core.Out_channel.close expected_channel;

                let (actual_name, actual_channel) = Filename.open_temp_file "actual" ".txt" in
                Core.Out_channel.output_string actual_channel execution_result;
                Core.Out_channel.close actual_channel;

                let command = Printf.sprintf "diff -w -u -W 80 %s %s | colordiff" (String.escaped expected_name) (String.escaped actual_name) in
                let in_channel = Unix.open_process_in command in
                print_string (Core.In_channel.input_all in_channel);
                Color_print.red_printf "\n\n[%s]\nThe tests have failed. Everything is terrible.\n" name;
                exit 1
            end
    in run tree ();
    Color_print.color_printf ~color:`Orange "\n        %s\n" (gen_fortune_cookie ())

let () =
    let test_tree = read_dir_test_files "./" in
    run_tests test_tree
