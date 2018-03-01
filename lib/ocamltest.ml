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
            let start_time = Sys.time () in
            let in_channel = Unix.open_process_in (
                "echo \"" ^ String.escaped test_info.input ^ "\" | " ^
                "../_build/default/bin/main.exe " ^ test_info.flags) in
            let execution_result = Core.In_channel.input_all in_channel in
            let success = (String.trim execution_result) = (String.trim test_info.output) in
            let symbol = if success then test_passed_symbol else test_failed_symbol in
            let finish_time = Sys.time () in
            let total_millis = (finish_time -. start_time) *. 1000.0 in
            Printf.printf "%s%s - %s %s\n"
                (indent level) symbol test_info.description
                (Color_print.yellow_sprintf "(%fms)" total_millis);
            if not success then begin
                Printf.printf "\n  -- Actual -- \n\n";
                Color_print.red_printf "%s" execution_result;
                Printf.printf "\n\n  -- Expected -- \n\n";
                Color_print.cyan_printf "%s" test_info.output;
                Color_print.yellow_printf "\n  THE TESTS FAILED BECAUSE [EVERYTHING IS TERRIBLE] (on file %s)\n" name;
                exit 1
            end
    in run tree ()

let () =
    let test_tree = read_dir_test_files "./" in
    run_tests test_tree
