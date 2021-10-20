#open "graphics";;

(*Dimension de la fenetre*)
let wl,wh = 522, 522;;
let window_size = " "^(string_of_int wl)^"x"^(string_of_int wh);;

(*Origine ramenee au center*)
let center_x, center_y = l/2, h/2;;

(*Type square: contient les coord. du center du square, et la longeur du edge*)
type square = {center: float*float; edge: float};;

let square_zero = {center=0.0,0.0; edge=0.};;

(*square de base, de edge 1/3, center en (0,0)*)
let first_square = {center=0.0,0.0; edge=0.333333333};;

(*list des squares de base*)
let squares = [first_square];;

let iof = int_of_float;;
let foi = float_of_int;;

(*Conversion point <->pixel*)
let point_to_pixel (x,y) = ( iof (x*.(foi (wl/2))) + wl/2, iof (y*.(foi (wh/2))) + wh/2);;

let line_to (x,y) =
	let a,b = point_to_pixel (x,y) in
	lineto a b;;

let move_to (x,y) =
	let a,b = point_to_pixel (x,y) in
	moveto a b;;

let point (x,y) =
	let a,b = point_to_pixel (x,y) in
	fill_circle a b 3;;

(*draw un square*)
let draw_square square =
	let a,b = (point_to_pixel square.center) in
	let w,h = iof (square.edge*.(foi wl)), iof (square.edge*.(foi wh)) in
	fill_rect (a-w/2) (b-h/2) w h;;

(*draw une list de squares*)
let rec draw_square_list l = match l with
	[]->()
	|t::q->draw_square t; draw_square_list q;;

(*Cree les 8 squares autour du square central*)
let add_square square =
	let tab = make_vect 8 square_zero in
	let ind = ref 0 in

	let (center_x, center_y) = square.center
	and	edge = square.edge in

	for i=0 to 2 do
		for j=0 to 2 do
			if (i,j) <> (1,1) then begin
				let ix = foi i and jy = foi j in
					tab.(!ind) <- {center= (center_x +. edge*.2. -. (ix *. 2. *. edge),
																 	center_y +. edge*.2. -. (jy *. 2. *. edge));
												edge = edge/.3.0};
					incr ind;
			end;
		done;
	done;
	list_of_vect tab;;

(*Ajoute les squares aux alentours sur une list de squares*)
let rec add_square_list l = match l with
	[]->failwith "Ne doit pas arriver"
	|[t]-> (add_square t)@l
	|t::q->(add_square t)@(add_square_list q);;

(*Division appliquee it fois*)
let rec sierpinski it l =
	if it=0 then l else sierpinski (it-1) (add_square_list l);;


open_graph window_size;;
let fractal = sierpinski 4 squares in
draw_square_list fractal;;
