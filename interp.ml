(* $Id: interp.ml,v 1.19 2021-04-20 20:19:30-07 - - $ *)

(*Jay Pandit, jpandit@ucsc.edu*)
(*Tanisha Khemka, tkhemka@ucsc.edu*)

open Absyn

let want_dump = ref false

let source_filename = ref ""

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> eval_memref memref
    | Unary (oper, expr) -> eval_unary oper expr
    | Binary (oper, expr1, expr2) -> eval_binary oper expr1 expr2

(*Helper function to calculate unary expressions*)
and eval_unary (oper : Absyn.oper) (expr : Absyn.expr) = 
    let num_value = (eval_expr expr) 
    in Hashtbl.find (*going to search within the unary function table in tables.ml and see if it exists*)
        Tables.unary_fn_table 
            oper num_value

(*Helper function to calculate binary expressions*)
and eval_binary (oper : Absyn.oper) (expr1 : Absyn.expr) (expr2 : Absyn.expr) =
    let num_valueOne = eval_expr expr1
    and num_valuetwo = eval_expr expr2
    in Hashtbl.find (*going to search within the binary function table in tables.ml and see if it exists*)
        Tables.binary_fn_table 
            oper num_valueOne num_valuetwo

(*retrieve variable and/or array from hashtables*)
and eval_memref (memref : Absyn.memref) : float = match memref with
    | Arrayref (ident, expr) -> 
        (*let convertToFloat = eval_expr expr in*)
        let name = (Hashtbl.find Tables.array_table ident) in
            Array.get name (int_of_float (eval_expr expr))
    | Variable ident -> try Hashtbl.find Tables.variable_table ident
                        with Not_found -> 0.0


and eval_STUB reason = (
    print_string ("(" ^ reason ^ ")");
    nan)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continue -> match firstline with
       | _, _, None -> interpret continue
       | _, _, Some stmt -> (interp_stmt stmt continue)

and interp_stmt (stmt : Absyn.stmt) (continue : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim ident expr continue
    | Let (memref, expr) -> interp_let memref expr continue
    | Goto label -> interp_goto label continue
    | If (relexpr, label) -> interp_if relexpr label continue
    | Print print_list -> interp_print print_list continue
    | Input memref_list -> interp_input memref_list continue

(*creates array same as asg1*)
and interp_dim (ident : Absyn.ident) (expr : Absyn.expr) 
               (continue : Absyn.program) = (
    let arraycreate = eval_expr expr
    in  Hashtbl.add Tables.array_table ident 
        (Array.make (int_of_float (arraycreate)) 0.);
        interpret continue)

(*let function*)
and interp_let (memref : Absyn.memref) (expr : Absyn.expr)
               (continue : Absyn.program) = 
    (*checks what type it is(variable or Arrayref)*)           
    match memref with
    | Variable ident -> 
    let value1 = eval_expr expr
    in  Hashtbl.add Tables.variable_table ident (value1);
        interpret continue
    | Arrayref (ident, arg) -> 
        let value2 = eval_expr expr
        and value3 = eval_expr arg
        and avalue = Hashtbl.find Tables.array_table ident 
        in  (Array.set avalue (int_of_float (value3))
                              (value2));
        interpret continue


and interp_goto (label : Absyn.label) 
                (continue : Absyn.program) =
    (*use label table to go to appropriate label*)            
    try 
        let label_value =  Hashtbl.find Tables.label_table label 
    in interpret label_value;
    with _ -> (exit 1) (*error handling purposes*)


and interp_if (relexpr: Absyn.relexpr) (label: Absyn.label) (continue : Absyn.program) = match relexpr with
    | Relexpr (oper, expr1, expr2) -> (*defining a relative expression based on a particular format and checking within boolean and label tables*)
        let ops = Hashtbl.find Tables.bool_fn_table oper
        and firstNumber = eval_expr expr1
        and secondNumber = eval_expr expr2
        in 
            if (ops firstNumber secondNumber)
            then
                try 
                    let findInTable = Hashtbl.find Tables.label_table label
                    in 
                        interpret findInTable
                with 
                    Not_found -> (exit 1);
            else 
                interpret continue

(*given*)
and interp_print (print_list : Absyn.printable list)
                 (continue : Absyn.program) =
    let print_item item = match item with
        | String string ->
          let regex = Str.regexp "\"\\(.*\\)\""
          in print_string (Str.replace_first regex "\\1" string)
        | Printexpr expr ->
          print_string " "; print_float (eval_expr expr)
    in (List.iter print_item print_list; print_newline ());
    interpret continue

(*had to check for input and see what type it is*)
and interp_input (memref_list : Absyn.memref list)
                 (continue : Absyn.program)  =
    let input_number memref =
        try let number = Etc.read_number () 
            in (match memref with (*very similar to memref logic*)
                | Variable ident -> 
                    (Hashtbl.add Tables.variable_table ident number)
                | Arrayref(ident, arg) ->
                    let value = Hashtbl.find Tables.array_table ident
                    in Array.set value (int_of_float(eval_expr arg)) 
                                        number;
                );
        with End_of_file -> (*error handling process*)
            (Hashtbl.add 
               Tables.variable_table "eof" 1.)
    in List.iter input_number memref_list;
    interpret continue

(*given*)
and interp_STUB reason continue = (
    print_string "Unimplemented: ";
    print_string reason;
    print_newline();
    interpret continue)

(*given*)
and interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)
