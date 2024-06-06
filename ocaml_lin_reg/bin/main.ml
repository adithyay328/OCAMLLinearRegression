(* These are the model paramaters *)

let m = ref 0.0
let b = ref 0.0

(* If you want model output, just run this *)
let model x = (!m *. x) +. !b

(* Takes mean of a list of numbers *)
let mean (inList : float list) : float = 
  let sum = ref 0.0 in

  List.iter( fun value -> sum := !sum +. value) inList;

  !sum /. float_of_int (List.length inList)

(* Print a list of flaots *)

(*
let listPrint ( inList : float list ) = 
  Printf.printf "[";
  List.iter( fun value -> Printf.printf "%f, " value ) inList;
  Printf.printf "]\n"
*)

(* Define cost function, which takes in a list x of inputs, and a list y of outputs *)
let cost xs ys = 
  let modelPred = List.map( model ) xs in
  let errorVector = List.map2 (fun pred gt -> ( ( gt -. pred) ** 2.0 ) ) modelPred ys in
  
  (* 
  let errorSum = ref 0.0 in
  let numElements = ref 0 in

  List.iter ( fun error ->
    errorSum := !errorSum +. error;
    numElements := !numElements + 1
  ) errorVector;

  !errorSum /. float_of_int !numElements
  *)
  mean errorVector

(* 
   This function computes the partial derviatives of cost
   w.r.t slope and intercept, and returns them in the list
   [dc/dm, dc/db]
*)
let gradient xs ys = 
  let modelPred = List.map( model ) xs in
  let innerErrorVector = List.map2 (fun pred gt -> ( ( gt -. pred) ) ) ys modelPred in

  (* 
     Multiply inner error vector times 2 now, since both
     calculations need a 2x multiplier
   *)

  let innerTimesTwo = List.map( fun inp -> inp *. 2.0 ) innerErrorVector in

  (*
   For partial m we need to multiply by -x, otherwise just -1
   *)
  let partialM = mean ( List.map2( fun x inner -> 1.0 *. x *. inner ) xs innerTimesTwo ) in
  let partialB = 1.0 *. (mean innerTimesTwo) in 

  [partialM; partialB]

let dataX = ref []
let dataY = ref []

let generate_data () = 
  let m_acc = (Random.float 5.0) -. 3. in
  let b_acc = (Random.float 5.0) -. 3. in

  Printf.printf "True m: %f, True b: %f\n" m_acc b_acc;

  for _ = 0 to 100 do
    let randX = (Random.float 20.) -. 10. in
    let yPred = m_acc *. randX +. b_acc in
    
    dataX := !dataX @ [randX];
    dataY := !dataY @ [yPred];
  done

(* Generate the data *)

let () = generate_data ()

(* Print the data *
Printf.printf "DataX: ";
listPrint !dataX;
Printf.printf "DataY: ";
listPrint !dataY
*)

let lr = 0.01
let numIters = 1000

(* Now, time to write out the training loop *)
let train () = 
  for _ = 0 to numIters do
    let currGrad = (gradient !dataX !dataY) in

    (*
    Printf.printf "Iteration %d: Cost = %f\n" trainIter (cost !dataX !dataY);

    Printf.printf "Gradient: ";
    listPrint currGrad;

    (* Also print params *)
    Printf.printf "m = %f, b = %f\n\n" !m !b;
    *)
    
    m := !m -. ( List.nth currGrad 0 ) *. lr;
    b := !b -. ( List.nth currGrad 1 ) *. lr;

  done

let () = train ();

Printf.printf "Final m: %f, Final b: %f\n" !m !b;
Printf.printf "Final cost: %f\n" (cost !dataX !dataY)
