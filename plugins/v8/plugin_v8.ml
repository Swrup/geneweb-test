open Geneweb
open Config
open Gwdb

open Plugin_v8_lib
module A = V8_HTML.ATTR
module T = V8_HTML.TAG
module G = V8_GUI

let skeleton assets head body =
  let head =
    T.head (T.link () ~attr:[ A.href "geneweb.css"
                            ; A.rel "stylesheet" ]
            :: T.meta () ~attr:[ "charset", "UTF-8" ]
            :: head)
  in
  [ T.html [ head
           ; T.body [ T.div ~attr:[ A.id "content" ] body ] ] ]

let render conf page =
  Util.html conf ; (* Send headers *)
  V8_HTML.RENDER.r conf page

module Handler = struct

  let getenv_list fn prefix conf =
    let rec loop acc i =
      match Util.p_getenv conf.env (prefix ^ string_of_int i) with
      | Some k -> loop (fn k :: acc) (i + 1)
      | None -> acc
    in
    loop [] 0

  let cpl fst conf base p fam =
    (if fst
     then T.span (G.IND.ind ~link:true ~sex:true conf base p)
     else T.span (G.IND.ind ~link:false ~sex:true conf base p))
    :: T.pcdata ", "
    :: T.span (G.FAM.union conf base p fam)
    :: []

  let children conf base ppi fam =
    match G.FAM.children conf base (fun c b p -> [ ppi c b p ]) fam with
    | [] -> T.pcdata ""
    | children -> T.ul children

  let desc =
    let rec desc ppf current max conf base p =
      let children fam =
        if current < max
        then children conf base (desc cpl (current + 1) max) fam
        else if Array.exists (fun i -> foi base i |> get_children <> [||]) (get_family p)
        then T.ul [ T.li ~attr:[ A.class_ "ellipsis" ] [ T.pcdata "…" ] ]
        else T.pcdata ""
      in
      match get_family p with
      | [||] -> T.span (G.IND.ind ~link:true ~sex:true conf base p)
      | [| i |] when current <> 0 ->
        let fam = foi base i in
        begin match children fam with
          | Pcdata "" ->
            T.div [ T.span (ppf true conf base p fam) ]
          | children ->
            T.div  [ T.span (ppf true conf base p fam)
                   ; T.pcdata " : "
                   ; children
                   ]
        end
      | arr ->
        T.ul begin Array.to_list @@ Array.mapi begin fun i ifam ->
            let fam = foi base ifam in
            match children fam with
            | Pcdata "" ->
              T.li (ppf (i = 0) conf base p fam)
            | children ->
              T.li [ T.span (ppf (i = 0) conf base p fam) ; children ]
          end arr
        end
    in desc (fun _ conf base p fam -> G.FAM.union conf base p fam)

  let pow2 n = 2. ** (float_of_int n) |> truncate

  let tree conf base p =
    let rec tree acc todo current max =
      if current < max && List.exists ((<>) None) todo then begin
        let attr = [ A.colspan (pow2 max / pow2 current |> string_of_int) ] in
        let dom, todo =
          List.map begin function
            | None ->
              ( T.td ~attr []
              , [ None ; None ]
              )
            | Some p ->
              ( T.td ~attr (G.IND.ind conf base p)
              , match get_parents p with
              | None -> [ None ; None ]
              | Some i ->
                let fam = foi base i |> gen_couple_of_family in
                [ Some (Adef.father fam |> poi base)
                ; Some (Adef.mother fam |> poi base)
                ]
              )
          end todo
          |> List.split
        in
        tree (T.tr dom :: acc) (List.flatten todo) (current + 1) max
      end else acc
    in match get_parents p with
    | Some i ->
      let fam = foi base i |> gen_couple_of_family in
      let todo =
        [ Some (Adef.father fam |> poi base) ; Some (Adef.mother fam |> poi base) ]
      in
      Some (T.table ~attr:[ A.class_ "tree" ] (tree [] todo 0 2))
    | None -> None

  let ind assets conf base p =
    let body =
      begin fun acc ->
        T.h1 (G.IND.designation ~link:true conf base p)
        :: T.a ~attr:[ G.href conf ("&m=MOD_IND&i=" ^ string_of_iper (get_iper p) ) ] [ T.pcdata "[mod.]" ]
        :: acc
      end @@ begin fun acc ->
        match tree conf base p with
        | None -> acc
        | Some tree ->
          T.h2 [ T.pcdata "v8_txt_ascendance_h2" ]
          :: tree
          :: acc
      end @@ begin fun acc ->
        if get_family p <> [||]
        then
          T.h2 [ T.pcdata "v8_txt_unions_h2" ]
          :: desc 0 3 conf base p
          :: acc
        else acc
      end @@ begin fun acc ->
        match Perso.events_list conf base p with
        | [] -> acc
        | events ->
          T.h2 [ T.pcdata "v8_txt_events_h2" ]
          :: T.table
               ~attr:[ A.class_ "timeline" ]
               begin List.map begin fun (name, date, place, note, src, wl, isp) ->
                   T.tr [ T.td [ match Adef.od_of_cdate date with
                       | None -> T.pcdata "-"
                       | Some d -> DateDisplay.string_of_date conf d
                                   |> Utf8.capitalize_fst
                                   |> T.pcdata ]
                        ; T.td (T.strong
                                  [ begin match name with
                                      | Perso.Pevent name -> Util.string_of_pevent_name conf base name
                                      | Fevent name -> Util.string_of_fevent_name conf base name
                                    end
                                    |> Utf8.capitalize_fst
                                    |> T.pcdata
                                  ]
                                ::
                                if empty_string = place then []
                                else [ T.br ()
                                     ; sou base place |> T.pcdata ]
                               ) ]
                 end events end
          :: acc
      end
        []
    in
    skeleton assets [] body
    |> render conf

  let rec range start stop fn =
    if start = stop then []
    else fn start :: range (start + 1) stop fn

  let mod_ind assets conf base p =
    let s get = get p |> sou base in
    let w_label label input =
      if label <> ""
      then
        T.div ~attr:[ A.class_ "form-group" ]
          [ T.label [ T.pcdata label ] ; input ]
      else input
    in
    let input ?(attr = []) label name value =
      T.input_text ~attr:(A.name name :: (value |> Util.escape_html |> A.value) :: attr) ()
      |> w_label (Utf8.capitalize_fst @@ Util.transl conf label)
    in
    let inputs label name values =
      let input ?(attr = []) name value =
        T.input ~attr:(A.name name
                      :: A.type_ "text"
                      :: (value |> Util.escape_html |> A.value)
                      :: attr) ()
      in
      (* let values = if values = [] then [""] else values in *)
      T.div ~attr:[ A.class_ "form-group" ]
        (T.label [ T.pcdata label ]
         :: (List.mapi (fun i v -> input (name ^ string_of_int i) v) values)
         @ G.FORM.extra (name ^ string_of_int (List.length values))
           [ input
               ~attr:[ A.class_ "input-extra" ]
               (name ^ string_of_int (List.length values)) "" ]
        )
    in
    let input_hidden name value =
      T.input_hidden ~attr:[ A.name name ; value |> Util.escape_html |> A.value ] ()
    in
    let textarea label name value =
      T.textarea ~attr:[ A.name name ] [ T.pcdata (Util.escape_html value) ]
      |> w_label label
    in
    let pevent ?attr i e =
      let i = string_of_int i in
      let name =
        input "v8_txt_ename" ("e_name" ^ i) ~attr:[ A.list "datalist-pevents" ] @@ match e.Def.epers_name with
        | Epers_Birth -> "v8_txt_Epers_Birth"
        | Epers_Baptism -> "v8_txt_Epers_Baptism"
        | Epers_Death -> "v8_txt_Epers_Death"
        | Epers_Burial -> "v8_txt_Epers_Burial"
        | Epers_Cremation -> "v8_txt_Epers_Birth"
        | _ -> ""
      in
      let date = input "v8_txt_edate" ("e_date" ^ i) "" in
      let place = input "v8_txt_edate" ("e_place" ^ i) (sou base e.epers_place) in
      let src = input "v8_txt_esrc" ("e_src" ^ i) (sou base e.epers_src) in
      let note = textarea "v8_txt_enote" ("e_note" ^ i) (sou base e.epers_note) in
      T.fieldset ?attr [ name ; date ; place ; src ; note ]
    in
    let body =
      let pevents = get_pevents p in
      [ T.form
          [ input_hidden "image" (s get_image)
          ; T.h1 (G.IND.designation conf base p)
          ; input "v8_txt_first_name" "first_name" (s get_first_name)
          ; input "v8_txt_surname" "surname" (s get_surname)
          ; input "v8_txt_occ" "occ" (get_occ p |> string_of_int)
          ; input "v8_txt_public_name" "public_name" (s get_public_name)
          ; input "v8_txt_occupation" "occu" (s get_occupation)
          ; inputs "v8_txt_qualifiers" "qualifier" (get_qualifiers p |> List.map (sou base))
          ; inputs "v8_txt_alias" "alias" (get_aliases p |> List.map (sou base))
          ; inputs "v8_txt_first_name_alias" "first_name_alias" (get_first_names_aliases p |> List.map (sou base))
          ; inputs "v8_txt_surname_alias" "surname_alias" (get_surnames_aliases p |> List.map (sou base))
          ; T.h2 [ T.pcdata "v8_txt_events" ]
          ; T.datalist ~attr:[ A.id "datalist-pevents" ] begin
              List.map begin fun k ->
                T.option ~attr:[ A.value k ]  ()
              end [ "EPERS_BIRTH" ; "EPERS_DEATH" ; "EPERS_BURIAL" ; "EPERS_CREMATION" ]
            end
          ; T.div begin List.mapi (pevent ~attr:[ A.class_ "event" ]) pevents
                        @  List.flatten begin range (List.length pevents) (List.length pevents + 10) begin fun i ->
                            G.FORM.extra (string_of_int i)
                              [ pevent ~attr:[ A.class_ "input-extra pevent" ] i
                                  { epers_name = Epers_Name Gwdb.empty_string
                                  ; epers_date = Adef.cdate_None
                                  ; epers_place = Gwdb.empty_string
                                  ; epers_reason = Gwdb.empty_string
                                  ; epers_note = Gwdb.empty_string
                                  ; epers_src = Gwdb.empty_string
                                  ; epers_witnesses = [||]
                                  }
                              ] end
                        end
            end
          ; T.button ~attr:[ A.type_ "submit" ; A.class_ "button" ]  [ T.pcdata "v8_txt_validate" ]
          ]
      ]
    in
    skeleton assets [] body
    |> render conf

  let alphabetic_order_sn base a b =
    let particle_at_the_end s =
      Util.surname_without_particle base s ^ Util.surname_particle base s
    in
    Gutil.alphabetic_order (particle_at_the_end a) (particle_at_the_end b)

  let np_a_aux sn s cnt conf base list =
    let sort =
      if sn
      then List.sort (fun a b -> alphabetic_order_sn base (s a) (s b))
      else List.sort (fun a b -> Gutil.alphabetic_order (s a) (s b))
    in
    T.ul begin List.map begin fun x ->
        let s = s x in
        let cnt = cnt x in
        T.li [ T.a ~attr:[ G.href conf ("&m="
                                        ^ (if sn then "N" else "P")
                                        ^ "&T=A&v="
                                        ^ Mutil.encode s ) ]
                 (if sn
                  then [ T.pcdata (Util.surname_without_particle base s)
                       ; T.pcdata (Util.surname_particle base s) ]
                  else [ T.pcdata s ])
             ; T.pcdata ( " (" ^ string_of_int cnt ^ ")")
             ]
      end (sort list) end

  let specify_aux sn conf ini keys =
    [ T.p begin List.map begin fun k ->
          T.a ~attr:[ if k = ini
                      then G.href conf ("&m=" ^ (if sn then "N" else "P") ^ "&T=A&v=" ^ Mutil.encode k)
                      else G.href conf ("&m=" ^ (if sn then "N" else "P") ^ "&T=A&k=" ^ Mutil.encode k)
                    ]
            [ T.pcdata (Mutil.tr '_' ' ' k) ]
        end keys end
    ; T.p [ T.pcdata "v8_txt_see_all"
          ; T.pcdata ":"
          ; T.a
              ~attr:[ G.href conf ("&m=" ^ (if sn then "N" else "P") ^ "&tri=A&o=A&k=" ^ Mutil.encode ini) ]
              [ T.pcdata "v8_txt_long_display" ] ]
          ; T.a
              ~attr:[ G.href conf ("&m=" ^ (if sn then "N" else "P") ^ "&tri=S&o=A&k=" ^ Mutil.encode ini) ]
              [ T.pcdata "v8_txt_short_display" ]
          ]

  let np_a sn assets conf base =
    let ini = match List.assoc_opt "k" conf.env with Some k -> k | _ -> "" in
    begin match Alln.select_names conf base sn ini 200 with
      | Alln.Specify keys, len -> specify_aux sn conf ini keys
      | Alln.Result list, len ->
        if len >= 50 || ini = ""
        then
          let list = Alln.groupby_ini (Utf8.length ini + 1) list in
          [ T.p begin List.map begin fun (ini, _) ->
                T.a ~attr:[ A.href ("#a" ^ ini) ] [ T.pcdata (Mutil.tr '_' ' ' ini) ]
              end list end
          ; T.ul begin List.map begin fun (ini, list) ->
                T.li [ T.a ~attr:[ A.href ("a" ^ ini) ] [ T.pcdata (Mutil.tr '_' ' ' ini) ]
                     ; np_a_aux sn fst snd conf base list
                     ]
              end list end
          ]
        else [ np_a_aux sn (fun (_, x, _) -> x)  (fun (_, _, x) -> x) conf base list ]
    end

  let np_f sn assets conf base =
    let (list, len) = Alln.select_names conf base sn "" max_int in
    let list = Alln.groupby_count list in
    List.map begin fun (cnt, list) ->
      T.li [ T.span ~attr:[ A.class_ "cnt" ] [ T.pcdata (string_of_int cnt) ]
           ; T.ul begin List.map begin fun s ->
                 T.li [ T.a ~attr:[ G.href conf ("&m="
                                                 ^ (if sn then "N" else "P")
                                                 ^ "&v="
                                                 ^ Mutil.encode (Name.lower s) ) ]
                          (if sn
                           then [ T.pcdata (Util.surname_without_particle base s)
                                ; T.pcdata (Util.surname_particle base s) ]
                           else [ T.pcdata s ])
                      ]
               end list end
           ]
    end list
    |> fun list ->
    skeleton assets [] [ T.input_checkbox ~attr:[ A.id "compact"
                                             ; A.class_ "compact-radio" ] ()
                       ; T.label ~attr:[ A.for_ "compact"
                                       ; A.class_ "compact-label"
                                       ; A.class_ "" ] [ T.pcdata "v8_txt_compact" ]
                       ; T.ul ~attr:[ A.class_ "alln" ] list ]
    |> render conf

  let alln sn assets conf base =
    match List.assoc_opt "tri" conf.env with
    | Some "F" -> np_f sn assets conf base ; true
    (* | Some "S" -> print_short conf base true *)
    | None | Some "A" ->
      np_a sn assets conf base
      |> skeleton assets []
      |> render conf ;
      true
    | _ -> false

  let some_fn assets conf base x =
    match Some.select_first_name conf base (List.assoc_opt "t" conf.env = Some "A") x with
    | [] -> false (* Not_found *)
    | [_, (strl, iperl)] ->
      let ps = List.map (Util.pget conf base) iperl in
      let ps = List.sort (fun x1 x2 -> Gutil.alphabetic (p_surname base x1) (p_surname base x2)) ps in
      let _acc =
        List.fold_left begin fun acc x ->
          let px = p_surname base x in
          match acc with
          | (p, l1) :: acc when Gutil.alphabetic px p = 0 -> (p, x :: l1) :: acc
          | _ -> (px, [x]) :: acc
        end [] ps
      in
      print_endline __LOC__ ; false (* Found *)
    | list ->
      print_endline __LOC__ ;
      [ T.ul begin List.map begin fun (sstr, (strl, _)) ->
            T.li [ T.a
                     ~attr:[ G.href conf ("&m=P&v=" ^ Mutil.encode sstr) ]
                     [ Some.StrSet.elements strl
                       |> String.concat ", "
                       |> T.pcdata ]
                 ]
          end list end
      ]
      |> skeleton assets []
      |> render conf ;
      true

  let some_sn assets conf base x =
    let (list, iperl, inj) = Some.select_surname conf base (List.assoc_opt "t" conf.env = Some "A") x in
    if List.assoc_opt "o" conf.env = Some "i"
    then
      List.filter_map begin fun p ->
        let p = Util.pget conf base p in
        if not (Util.is_hide_names conf p) || Util.authorized_age conf base p
        then Some p
        else None
      end iperl
      |> begin function [] -> (* surname_not_found conf x *) false
                      | x -> false
      end
      (* |> print_family_alphabetic x conf base *)
    else
      let bhl = Some.mk_branches conf base inj iperl in
      match bhl, list with
      | [], _ -> (* not_found_fun conf x *) false
      | _, [s, (strl, _)] ->
        (* print_one_surname_by_branch conf base x strl (bhl, s) *) false
      | _ ->
        (* print_several_possible_surnames x conf base (bhl, strl) *) false

end

let ns = "v8"

let misc = fun assets conf -> function
  | "geneweb.css" as fname ->
    let ic = Secure.open_in_bin (Filename.concat assets fname) in
    let len = in_channel_length ic in
    Output.status conf Def.OK;
    Output.header conf "Content-type: text/css";
    Output.header conf "Content-length: %d" len;
    Output.header conf "Content-disposition: inline; filename=%s" (Filename.basename fname);
    really_input_string ic len |> Output.print_string conf ;
    Output.flush conf ;
    close_in ic;
    true
  | _ -> false

let w_base fn conf = function Some base ->  fn conf base
                            | None -> false

(* taken from stdlib
 * TODO: replace by Array.combine once we get to ocaml>=4.13 *)
let array_combine a b =
  let open Array in
  let na = length a in
  let nb = length b in
  if na <> nb then invalid_arg "Array.combine";
  if na = 0 then [||]
  else begin
    let x = make na (unsafe_get a 0, unsafe_get b 0) in
    for i = 1 to na - 1 do
      unsafe_set x i (unsafe_get a i, unsafe_get b i)
    done;
    x
  end

let pop_pyr_handler assets conf base =
  let open Util in
  let open Def in
  let open BirthDeath in
  match base with
  | None ->
    Printf.eprintf "Invalid request: no base";
    false
  | Some base ->
    let interval =
      match p_getint conf.env "int" with None -> 5 | Some i -> max 1 i
    in
    let limit = match p_getint conf.env "lim" with None -> 0 | Some x -> x in
    let at_date =
      match p_getint conf.env "y" with
      | None -> conf.today
      | Some i -> { year = i; month = 31; day = 12; prec = Sure; delta = 0 }
    in
    let humans =
      let men, wom =
        make_population_pyramid ~nb_intervals:(150 / interval) ~interval ~limit
          ~at_date conf base
      in
      let humans = array_combine men wom in
      (* remove the lasts empty intervals *)
      Array.fold_right
        (fun ((nb_men, nb_wom) as v) acc ->
          (* if it's empty we have seen only 0s and should keep on removing intervals, otherwise we should stop removing them *)
          if acc = [] && nb_men = 0 && nb_wom = 0 then [] else v :: acc )
        humans []
    in

    let half_row_to_string =
      let rec count_digit n =
        if n < 0 then
          if n > -10 then 2 (* minus sign *) else 1 + count_digit (n / 10)
        else if n < 10 then 1
        else 1 + count_digit (n / 10)
      in
      let max_hum =
        List.fold_left (fun acc (a, b) -> max acc (max a b)) 1 humans
      in
      let date_size = count_digit max_hum in
      let max_number_sign = 20 in
      let nb_of_number_sign n =
        let nb = n * max_number_sign / max_hum in
        min nb max_number_sign
      in
      fun ~reverse nb ->
        let nb_number_sign = nb_of_number_sign nb in
        let number_signs = String.make nb_number_sign '#' in
        let nb_whitespace =
          date_size - count_digit nb + max_number_sign - nb_number_sign
        in
        let whitespace =
          let non_breaking_space = "&nbsp;" in
          let buf =
            Buffer.create (nb_whitespace * String.length non_breaking_space)
          in
          for _i = 0 to nb_whitespace do
            Buffer.add_string buf non_breaking_space
          done;
          Buffer.contents buf
        in

        if reverse then Printf.sprintf "  %s %d%s" number_signs nb whitespace
        else Printf.sprintf "%s%d %s  " whitespace nb number_signs
    in

    let make_row date nb_men nb_wom =
      let td_date = T.td [ T.pcdata (string_of_int date) ] in
      let td_men =
        T.td [ T.pcdata (half_row_to_string ~reverse:false nb_men) ]
      in
      let td_wom =
        T.td [ T.pcdata (half_row_to_string ~reverse:true nb_wom) ]
      in
      let td_age =
        T.td [ T.pcdata (string_of_int (at_date.year - date + interval)) ]
      in
      T.tr [ td_date; td_men; td_age; td_wom; td_date ]
    in

    let rows =
      List.rev
      @@ List.mapi
           (fun i (nb_men, nb_wom) ->
             make_row (at_date.year - (i * interval)) nb_men nb_wom )
           humans
    in
    let title = Printf.sprintf "Population pyramid (%d)" at_date.year in
    let body = [ T.h1 [ T.pcdata title ]; T.table rows ] in
    let page = skeleton assets [] body in
    render conf page;
    true

let sosa_handler assets conf base =
  let error_page msg =
    let title = Printf.sprintf "Incorrect request: %s" msg in
    let body = [ T.h1 [ T.pcdata title ] ] in
    let page = skeleton assets [] body in
    let () = render conf page in
    false
  in

  match base with
  | None -> error_page "no base"
  | Some base -> (
    match Util.find_person_in_env conf base "" with
    | None -> error_page "invalid person"
    | Some person -> (
      match Util.p_getint conf.env "sosa" with
      | None -> error_page "invalid sosa"
      | Some n when n < 1 -> error_page "invalid sosa"
      | Some n -> (
        let get f person =
          let open Gwdb in
          match get_parents person with
          | None -> None
          | Some ifam -> foi base ifam |> f |> poi base |> Option.some
        in

        let rec sosa n path =
          if n = 1 then path
          else
            sosa (n / 2)
              ( (if n mod 2 = 0 then Gwdb.get_father else Gwdb.get_mother)
              :: path )
        in

        let l = sosa n [] in
        let ancestor_opt =
          List.fold_left
            (fun person_opt f -> Option.bind person_opt (get f))
            (Some person) l
        in
        match ancestor_opt with
        | None -> error_page "couldn't find ancestor"
        | Some ancestor ->
          let first_name = Gwdb.sou base (Gwdb.get_first_name ancestor) in
          let surname = Gwdb.sou base (Gwdb.get_surname ancestor) in
          let title = Printf.sprintf "%s %s" first_name surname in
          let body = [ T.h1 [ T.pcdata title ] ] in
          let page = skeleton assets [] body in
          render conf page;
          true ) ) )

let () =
  let module H = Handler in
  print_endline (__LOC__ ^ ": " ^ !Gwd_lib.GwdPlugin.assets) ;
  Secure.add_assets !Gwd_lib.GwdPlugin.assets ;
  Gwd_lib.GwdPlugin.register_misc ~ns misc ;
  Gwd_lib.GwdPlugin.register ~ns
    [ "", begin fun assets conf -> function
          | Some base ->
            begin match Util.find_person_in_env conf base "" with
              | Some p -> H.ind assets conf base p
              | None -> render conf [ G.logo ]
            end ;
            true
          | None ->
            render conf [ G.logo ] ;
            true
        end
    ; "MOD_IND", begin fun assets conf -> function
          | Some base ->
            begin match Util.find_person_in_env conf base "" with
              | Some p -> H.mod_ind assets conf base p
              | None -> assert false
            end ;
            true
          | None ->
            assert false
      end
    ; "P", begin fun assets -> w_base @@ fun conf base -> match Util.p_getenv conf.env "v" with
        | Some s -> H.some_fn assets conf base s
        | _ -> H.alln false assets conf base
      end
    ; "N", begin fun assets -> w_base @@ fun conf base -> match Util.p_getenv conf.env "v" with
        | Some s -> H.some_sn assets conf base s
        | _ -> H.alln true assets conf base
      end
    ; "POP_PYR", pop_pyr_handler
    ; "SOSA", sosa_handler
    ];
