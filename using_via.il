
;##################################################################### 
axlCmdRegister("setfunckey" `setfunckey) 
set_funckey = nil
(defun setfunckey ()
	ip_set_funckey = axlEnterString(?prompts list("Please enter line Clearance:"))
	if(ip_set_funckey != nil then
		case(ip_set_funckey
			("veday"
				axlSetFunckey( "5" "cre_cline_2via" t)
				axlSetFunckey( "6" "avc" t)
				axlSetFunckey( "7" "get_pin_net" t)
				axlSetFunckey( "8" "rats net" t)
				axlSetFunckey( "4" "net schedule" t)
				axlSetFunckey( "3" "join_cline" t)
				axlSetFunckey( "2" "guard_shape_chk" t)	
				axlSetFunckey( "1" "cut_void_shape" t)	
			)
			("guard"
				axlSetFunckey( "7" "hl_via_guard2" t)
				axlSetFunckey( "8" "cut_shape" t)
				axlSetFunckey( "6" "uni_via" t)
				axlSetFunckey( "3" "vvia" t)
				axlSetFunckey( "5" "uni_via7" t)
				axlSetFunckey( "4" "uni_via3" t)
				axlSetFunckey( "2" "guard_via_cline" t)	
			)
		)
	)	
)
;#####################################################################
axlCmdRegister("unviabtn" `unviabtn)
 (defun unviabtn ()
	ori_vis = axlVisibleGet()
	axlVisibleDesign(nil)
	axlVisibleLayer("VIA CLASS" t)
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	axlAddSelectAll()
	via_list = setof(e axlGetSelSet() index(e->name "BOTTOM") != nil )
	axlDBDeleteProp(via_list  list( "FIXED")) 
	axlVisibleSet(ori_vis) 
 
 )
;#####################################################################
 axlCmdRegister("fviabtn" `fviabtn)
 (defun fviabtn ()
	ori_vis = axlVisibleGet()
	axlVisibleDesign(nil)
	axlVisibleLayer("VIA CLASS" t)
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	axlAddSelectAll()
	via_list = setof(e axlGetSelSet() index(e->name "BOTTOM") != nil )
	axlDBAddProp(via_list  list( "FIXED" t)) 
	axlVisibleSet(ori_vis) 
 
 )
;#####################################################################
axlCmdRegister("unviatop" `unviatop)
 (defun unviatop ()
	ori_vis = axlVisibleGet()
	axlVisibleDesign(nil)
	axlVisibleLayer("VIA CLASS" t)
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	axlAddSelectAll()
	via_list = setof(e axlGetSelSet() index(e->name "TOP") != nil )
	axlDBDeleteProp(via_list  list( "FIXED")) 
	axlVisibleSet(ori_vis) 
 
 )
;#####################################################################
 axlCmdRegister("fviatop" `fviatop)
 (defun fviatop ()
	ori_vis = axlVisibleGet()
	axlVisibleDesign(nil)
	axlVisibleLayer("VIA CLASS" t)
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	axlAddSelectAll()
	via_list = setof(e axlGetSelSet() index(e->name "TOP") != nil )
	axlDBAddProp(via_list  list( "FIXED" t)) 
	axlVisibleSet(ori_vis) 
 
 )
;##################################################################### 
axlCmdRegister("check_conn_coa" `check_conn_coa)
(defun check_conn_coa ()
	ori_vis = axlVisibleGet()
	axlVisibleDesign(nil)
	axlVisibleLayer("VIA CLASS" t)
	Out_File = outfile("./check_conn_coa.log" "w")
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	axlAddSelectAll()
	via_list = setof(e axlGetSelSet() e->prop->COMMENT != nil && index( e->prop->COMMENT "Coaxtial") != nil )
	sub_list = axlSubclassRoute(?field 'isEtch)
	etch_sub_list = nil
	foreach(sub sub_list etch_sub_list = cons(strcat("ETCH/" sub) etch_sub_list) )
	etch_sub_list = reverse(etch_sub_list)
	foreach(via via_list
		via_st_end = via->startEnd
		tmp_list = member(car(via_st_end) etch_sub_list)
		tmp_list = reverse(member(cadr(via_st_end) reverse(tmp_list)))
		if(length(axlDBGetConnect(via t)) != length(tmp_list) then
			fprintf(Out_File "%L\n" via->xy )
			fprintf(Out_File "%L %L\n" length(axlDBGetConnect(via t)) tmp_list )
		
		)
		
		
		
		
	)
	close(Out_File)
	 view = axlUIViewFileCreate("check_conn_coa.log", " " nil)
	 axlVisibleSet(ori_vis) 
	
)
;##################################################################### 
axlCmdRegister("delete_via_pro" `delete_via_pro)
(defun delete_via_pro ()
	ip_delete_via_pro = upperCase(axlEnterString(?prompts list("Please enter Length need check :")))
	net_list = setof(e axlDBGetDesign()->nets index(e->name "SRD_AGND") != nil )
	via_list = nil
	via_list2 = nil
	foreach(net net_list
		foreach(branch net~>branches
			via_list_tmp =  setof(e branch->children (e->objType == "via" && axlDBIsFixed(e->net) == nil) && e->prop->DYN_THERMAL_CON_TYPE =="FULL_CONTACT" )
			foreach(via via_list_tmp
				tmp = setof(e axlDBGetConnect(via t) e->objType=="shape" && index(e->layer ip_delete_via_pro) != nil)
					if(car(tmp) == nil then 
						via_list2 = cons(via via_list2)
					)
				
				)
	   
			)
		)
		axlDeleteObject(via_list2)
	)

	
;##################################################################### 
axlCmdRegister("stop_deep_via" `stop_deep_via)
(defun stop_deep_via ()
	sub_list = nil
	foreach(item parseString(axlClipboardGetText() "\n")
		item = axlStringRemoveSpaces(item)
		if(index(item "\t") != nil then
			par_str = parseString(item "\t")
			if(cadr(par_str) != nil then 
				sub_list = cons(cadr(par_str) sub_list)
			)
		else
			sub_list = cons(item sub_list)
		)	
	)
	sub_list = setof(e sub_list member(e axlSubclassRoute(?field 'isEtch)) != nil)
	STT = 0
	view = nil
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	while(axlSelect(?prompt "Please select an element...") 
		Input_List = nil
		Input_List = axlGetSelSet()
		Input_List  = setof(e Input_List axlDBIsFixed(e->net) == nil)
		Input_List  = setof(e Input_List axlDBIsFixed(e) == nil)
		axlClearSelSet()
		if( view != nil then axlUIWClose(view))
		Out_File = outfile("./stop_deep_via.log" "w")
		STT = 0
		;sub_info = caar(setof(e car(setof(e axlVisibleGet() member("ETCH" e) != nil ))->subclassinfo cadr(e)== t))
		;cut_sub_list = setof(e member(sub_info axlSubclassRoute(?field 'isEtch)) e!="BOTTOM")
		;cut_sub_list = setof(e cut_sub_list e!=sub_info)
		ori_visible = axlVisibleGet()
		foreach(via setof(e Input_List axlDBIsFixed(e->net) == nil && index(e->name "TOP") != nil )
			end_via = cadr(parseString(cadr(via->startEnd) "/"))
			cut_sub_list = setof(e member(end_via axlSubclassRoute(?field 'isEtch)) e!="BOTTOM")
			cut_sub_list = setof(e cut_sub_list e!=end_via)
			poly = car(axlPolyFromDB(via ?endCapType 'ROUND ?layer strcat("ETCH/" end_via) ?padType 'REGULAR))
			Item_poly_Expand = car(axlPolyExpand(poly 112.5 'NONE))
			;shape = car(axlDBCreateShape( Item_poly_Expand t "ANTI ETCH/ALL" nil nil))
			tmp_list = nil
			exit_loop = nil
			if(cut_sub_list != nil then 
				foreach(sub cut_sub_list
					axlVisibleDesign(nil)
					axlVisibleLayer(strcat("ETCH/"  sub) t)
					axlVisibleLayer(strcat("VIA CLASS/"  sub) t)
					axlClearSelSet()
					axlSetFindFilter(?enabled '(noall VIAS CLINESEGS NAMEFORM) ?onButtons '(noall VIAS CLINESEGS ))
					axlAddSelectBox(Item_poly_Expand->bBox)
					get_db_all = axlGetSelSet()
					has_item = nil
					if(get_db_all != nil then 
						d_actual = 50.0
						d = axlMXYAdd( 0:d_actual via->xy) 
						r_path = axlMakeDynamicsPath(list(list(d d 1.0 via->xy d_actual t) list(d d) ) )
						poly1 = car(axlPolyFromDB(r_path ?layer "ANTI ETCH/ALL" ?endCapType 'ROUND))
						axlVisibleLayer(strcat("ANTI ETCH/"  sub) t)
						shape = car(axlDBCreateShape( poly1 t strcat("ANTI ETCH/"  sub) nil nil))
						get_db_all = setof(e get_db_all axlAirGap( shape e nil 'anyLayer) <137.5 )
						if(exit_loop == nil then 
							;fprintf(Out_File "%L %L\n" car(get_db_all) sub)
							if(car(get_db_all) != nil then
								exit_loop = t
							else
								has_item = t
								
							)
						)
						axlDeleteObject(shape)
					else
						if(exit_loop == nil then
							has_item = t
							
						)
					)
					if(has_item == t then
						axlVisibleDesign(nil)
						axlVisibleLayer(strcat("ETCH/"  sub) t)
						axlClearSelSet()
						axlSetFindFilter(?enabled '(noall SHAPES NAMEFORM) ?onButtons '(noall SHAPES ))
						axlAddSelectBox(Item_poly_Expand->bBox)
						get_shape = setof(e axlGetSelSet() e->net == via->net)
						if(car(get_shape) != nil then
							tmp_list = cons(sub tmp_list)
						)
					)
					
					
				)
				
				if(tmp_list != nil then 
					if(sub_list != nil then
						tmp_list = setof(e tmp_list member(e sub_list) != nil)
					)
					if(tmp_list != nil then 
						end_via_tmp = lastelem(tmp_list)
						;println(tmp_list)
						;println(via->name)
						cus_via_name = strcat(car(parseString(via->name "-")) "-" cadr(parseString(via->name "-")) "-" end_via_tmp)
						if(axlLoadPadstack(cus_via_name) != nil && via->name != cus_via_name  then 
							fprintf(Out_File "%-5n %L %L %L\n"  STT via->startEnd cus_via_name via->xy)
							STT++
							axlReplacePadstack(via cus_via_name)
							
						)
					)
				)
					
			)
		)
		axlVisibleSet(ori_visible)
		close(Out_File)
		axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
		if(STT>0 then 
		view = axlUIViewFileCreate("stop_deep_via.log", "Show Element To Check Cline" nil) )
	)

)
;##################################################################### 
axlCmdRegister("via_change_2pin" `via_change_2pin)
(defun via_change_2pin ()
	sym = car(setof(e axlDBGetDesign()->symbols e->refdes=="BGA"))
	tmp_list = nil
	foreach(pin sym->pins
		con_pins = setof(e axlDBGetConnect(pin t) e->objType=="via" && index(e->name "-MP2-") == nil)
		tmp_list = cons(list(pin->number  con_pins~>name) tmp_list)
	)
	designName = axlCurrentDesign()
	axlOpenDesign(?design nil ?mode "wl" ?noMru nil)
	 Out_File = outfile("./via_change_2pin.log" "w")
	sym = car(setof(e axlDBGetDesign()->symbols e->refdes=="BGA"))
	STT = 0
	foreach(pin sym->pins
		find_pin = car(setof(e tmp_list car(e) == pin->number))
		con_pins = setof(e axlDBGetConnect(pin t) e->objType=="via" && index(e->name "-MP2-") == nil)
		if(car(con_pins) != nil then 
			if(cadr(find_pin) != con_pins~>name then
				STT++
				fprintf(Out_File "%-5n %L %L %L\n" STT unique(con_pins~>name) unique(cadr(find_pin)) pin->xy)
			)
		)
		
	)
	close(Out_File)
	axlOpenDesign(?design designName ?mode "wl" ?noMru nil)

)
;##################################################################### 
axlCmdRegister("counter_via_50" `counter_via_50)
(defun counter_via_50 ()
	clip_board_list = nil
	foreach(row parseString(axlClipboardGetText() "\n")
		row = axlStringRemoveSpaces(row)
		clip_board_list = cons(row clip_board_list)
	)
	p = axlSelectByName("NET" clip_board_list)
	;p = setof(e p member(e->name list("GND" "VCCINT" "VSS" "VDD" "VDDPST" "VSSPST" "VDDPST3318" "VSSPST33" "VDDQ[1]" 
	;"VSSPST" "VSSPST33" "VSSQ[0]" "GNDDC" "GND" "GNADC" "SRIO_GD" "VDDPST" "VCCO_46"
	;)) == nil)
	;println(clip_board_list)
	if(p != nil then
		via_list = nil
		foreach(branch p~>branches
			foreach(item branch
				 via_list_tmp =  setof(e item->children (e->objType == "via"))
				foreach(via via_list_tmp
					via_list = cons(via via_list)
				)
			)   
		)
		etch_shape_list = setof(e axlDBGetShapes("ETCH") (e->net->name !=""&&  e->symbolEtch== nil))
		Out_File = outfile("./counter_via_50.log" "w")
		foreach(net p~>net
			same_net_list = setof(e via_list e->net == net)
			at_top_list = setof(e same_net_list index(e->name "TOP")!= nil)
			at_btm_list = setof(e same_net_list index(e->name "BOTTOM")!= nil)
			;at_btm_list = setof(e same_net_list member(e at_top_list) == nil)
			if(length(at_top_list)>0 then 
				if(length(at_btm_list)>0 then 
					;fprintf(Out_File "%-5n %-5n %-10s\n" length(at_top_list) length(at_btm_list) net->name)
					shape_name_net = setof(e etch_shape_list e->net==net)
					top_counter2 = length(at_top_list)/2
					btm_counter2 = length(at_btm_list)/2
					foreach(shape shape_name_net
						shape_conn_vias = setof(e axlDBGetConnect(shape t) e->objType=="via")
						at_top_list2 = length(setof(e shape_conn_vias index(e->name "TOP")!= nil))
						at_btm_list2 = length(shape_conn_vias) - at_top_list2
						chk1= at_top_list2 -top_counter2
						chk2= at_btm_list2 -btm_counter2
						;if(at_top_list2 >0 then 
							;if(at_top_list2 <top_counter2 || at_btm_list2 <btm_counter2 then 
								fprintf(Out_File "%-5s %-5s\n" "DIE" "LGA")
								fprintf(Out_File "%-5n %-5n\n" length(at_top_list) length(at_btm_list))
								fprintf(Out_File "%-5n %-5n %-10s %-10s\n\n"  at_top_list2 at_btm_list2 net->name cadr(parseString(shape->layer "/")))
							;)
						;)
					)
				)
			)
			
		)
		close(Out_File)
		view = axlUIViewFileCreate("counter_via_50.log", "Show Element To Check Cline" nil)
	)

)
;##################################################################### 
axlCmdRegister("PB_NC2362_chk2" `PB_NC2362_chk2)
(defun PB_NC2362_chk2 ()
	 view = nil
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	while(axlSelect(?prompt "Please select an element...") 
		Input_List = nil
		Input_List = axlGetSelSet()
		axlClearSelSet()
		if( view != nil then axlUIWClose(view))
		Out_File = outfile("./PB_NC2362_chk2.log" "w")
		STT = 0
		foreach(via setof(e Input_List index(e->name "VIA75-") != nil )
			tmp = setof(e axlDBGetConnect(via t) e->objType=="path")
			if(length(tmp)>2 then
				fprintf(Out_File "%-10s %L\n" via->name via->xy )
			)
		)
		close(Out_File)
		view = axlUIViewFileCreate("PB_NC2362_chk2.log", "Show Element To Check Cline" nil)
	)

)
;##################################################################### 
axlCmdRegister("viaonvia" `viaonvia)
(defun viaonvia ()
	view = nil
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	while(axlSelect(?prompt "Please select an element...") 
		if( view != nil then axlUIWClose(view))
		Out_File = outfile("./viaonvia.log" "w")
		Input_List = nil
		Input_List = axlGetSelSet()
		axlClearSelSet()
		sub_info_via = car(setof(e axlVisibleGet() member("VIA CLASS" e) != nil ))->subclassinfo
		sub_info_via = caar(setof(e sub_info_via cadr(e)== t))
		tmp_list = nil
		foreach(via Input_List
			via_poly = car(axlPolyFromDB(via ?layer sub_info_via ?endCapType 'ROUND))
			axlClearSelSet()
			axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
			axlAddSelectBox(via_poly->bBox)
			tmp = setof(e axlGetSelSet() e->xy != via->xy)
			
			if(tmp != nil then
				tmp_list  = nconc(tmp_list  tmp~>xy) 
				
			)
			
		)
		foreach(item unique(tmp_list)
			fprintf(Out_File "%L\n" item)
		)
		close(Out_File)
		view = axlUIViewFileCreate("viaonvia.log", "Show Element To Check Cline" nil)
	)
)
;##################################################################### 
axlCmdRegister("sel_via_shape" `sel_via_shape)
(defun sel_via_shape ()
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall  SHAPES NAMEFORM) ?onButtons '(noall SHAPES))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	while(axlSelect(?prompt "Please select an element...") 
		Input_List = nil
		Input_List = axlGetSelSet()
		axlClearSelSet()
		if( view != nil then axlUIWClose(view))
		Out_File = outfile("./sel_via_shape.log" "w")
		via_list = nil
		net_list = nil
		STT =0
		foreach(shape Input_List
			shape_poly = car(axlPolyFromDB(shape ?endCapType 'ROUND))
			axlClearSelSet()
			axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
			axlAddSelectBox(shape_poly->bBox)
			;axlDBCreateRectangle( shape_poly->bBox t "ANTI ETCH/MP2")
			;axlDBCreateShape( shape_poly t "ANTI ETCH/MP3" nil)
			;axlAddSelectPoly(shape_poly t)
			;net_via_list = setof(e unique(axlGetSelSet()~>net~>name) e->net->name=="GND" )
			net_via_list = setof(e unique(axlGetSelSet()~>net~>name) e!= nil && index(e "FAB_") == nil )
			net_via_list = setof(e net_via_list index(e "NIF_") == nil )
			;AllViaPadstacks = nconc(AllViaPadstacks Via_outside_db) 
			via_list = cons(list(axlDBAltOrigin('center shape) axlGetSelSet() ) via_list)
			net_list = nconc(net_list net_via_list) 
			;foreach(via setof(e axlGetSelSet() axlGeoPointInShape(e->xy shape_poly nil) == t && e->net->name=="GND" && e->prop->COMMENT != nil )
			;foreach(via setof(e axlGetSelSet() axlGeoPointInShape(e->xy shape_poly nil) == t && index( e->name "TOP" ) != nil)
				;STT++
				;fprintf(Out_File "%-5d %-8s %-15s %-15s %L\n" STT cadr(parseString(shape->layer "/")) via->net->name via->prop->COMMENT via->xy )
				;fprintf(Out_File "pick %L\n"  via->xy )
			;)
			
			
		)
		net_list = unique(net_list)
		foreach(net net_list
			foreach(item via_list
				tmp = list(93 68 56 34 10)
				q = length(setof(e cadr(item) e->net->name==net))
				;if(member(q tmp) != nil then
					fprintf(Out_File "%-5d %-5d %-15s %L\t" length(cadr(item)) q net car(item))
				;)
				
			)	
			fprintf(Out_File "\n\n")
		)
		
		axlSetFindFilter(?enabled '(noall  SHAPES NAMEFORM) ?onButtons '(noall SHAPES))
		close(Out_File)
		view = axlUIViewFileCreate("sel_via_shape.log", "Show Element To Check Cline" nil)
	)

)
;##################################################################### 
axlCmdRegister("stretch_via" `stretch_via)
(defun stretch_via ()
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	while(axlSelect(?prompt "Please select an element...") 
		Input_List = nil
		Input_List = axlGetSelSet()
		axlClearSelSet()
		sub_info_via = car(setof(e axlVisibleGet() member("VIA CLASS" e) != nil ))->subclassinfo
		sub_info_via = caar(setof(e sub_info_via cadr(e)== t))
		tmp_list = nil
		Out_File = outfile("./stretch_via.log" "w")
		fprintf(Out_File "version 17.4\n" )
		fprintf(Out_File "setwindow pcb\n" )
		fprintf(Out_File "trapsize 514\n" )
		fprintf(Out_File "rfedit_appm\n" )
		fprintf(Out_File "move\n" )
		
		foreach(via Input_List
			via_poly = car(axlPolyFromDB(via ?layer sub_info_via ?endCapType 'ROUND))
			axlClearSelSet()
			axlSetFindFilter(?enabled '(noall  PINS NAMEFORM) ?onButtons '(noall PINS))
			axlAddSelectBox(via_poly->bBox)
			pin_list = car(axlGetSelSet())
			if(pin_list != nil  && via->xy != pin_list->xy then
				fprintf(Out_File "pick grid %f %f\n" car(via->xy ) cadr(via->xy ))
				fprintf(Out_File "pick grid %f %f\n" car(pin_list->xy ) cadr(pin_list->xy ))
			)	
		)
		axlClearSelSet()
		axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
		fprintf(Out_File "prepopup -12762.19 12586.65\n" )
		fprintf(Out_File "done\n" )
		fprintf(Out_File "rfedit_appm\n" )
		close(Out_File)
		axlUIViewFileCreate("stretch_via.log", "Show Element To Check Cline" nil)
	)

)
;##################################################################### 
axlCmdRegister("t_cline_via" `t_cline_via)
(defun t_cline_via ()
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	while(axlSelect(?prompt "Please select an element...") 
		Input_List = nil
		Input_List = axlGetSelSet()
		axlClearSelSet()
		sub_info_via = car(setof(e axlVisibleGet() member("VIA CLASS" e) != nil ))->subclassinfo
		sub_info_via = caar(setof(e sub_info_via cadr(e)== t))
		foreach(via Input_List
			via_poly = car(axlPolyFromDB(via ?layer sub_info_via ?endCapType 'ROUND))
			axlClearSelSet()
			axlSetFindFilter(?enabled '(noall  CLINESEGS NAMEFORM) ?onButtons '(noall CLINESEGS))
			axlAddSelectBox(via_poly->bBox)
			seg_list = axlGetSelSet()
			tmp_list = nil
			foreach(seg seg_list
				seg_st = seg->startEnd
				dis1 = axlDistance(via->xy car(seg_st))
				dis2= axlDistance(via->xy cadr(seg_st))
				point = car(seg_st)
				if(dis1<dis2 then
					point = cadr(seg_st)
				)
				tmp_list = cons(list(via->xy point seg->width seg->layer seg->net->name seg) tmp_list)
			)
			
			foreach(item tmp_list
				axlDeleteObject(lastelem(item))
				axlDBCreateLine( list(car(item) cadr(item)), nth(2 item), nth(3 item) nth(4 item) )
			)	
			axlDBRefreshId(via)
			tmp = setof(e axlDBGetConnect(via t) e->objType=="path" && length(axlDBGetConnect(e t))<2 )
			if(tmp != nil then
				axlDeleteObject(tmp)
			)
			
			
			
		)
		axlShell("redraw")
		axlClearSelSet()
		axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
	)


)
;##################################################################### 
axlCmdRegister("find_return_via" `find_return_via)
(defun find_return_via ()
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall VIAS  NAMEFORM) ?onButtons '(noall VIAS))
	name_file_list = nil
	foreach(item parseString(axlClipboardGetText() "\n")
		item = axlStringRemoveSpaces(item)
		row = item
		if(index(item "\t") != nil then
			row  = car(parseString(item "\t"))	
		)	
		name_file_list	= cons(row name_file_list)	
	)
	via_list = nil
	p = axlSelectByName("NET" name_file_list)
	foreach(branch p~>branches
		foreach(item branch
			 via_list_tmp =  setof(e item->children (e->objType == "via"))
			foreach(via via_list_tmp
				via_list = cons(via via_list)
			)
		)   
	)
	Out_File = outfile("./find_return_via.log" "w")
	foreach(sub axlSubclassRoute(?field 'isEtch ?value nil)
		via_at_layer = setof(e via_list axlPadOnLayer(e  sub) == t)
		foreach(net  name_file_list
			net_tmp = setof(e via_at_layer e->net->name==net)
			if(net_tmp != nil then
				tmp = setof(e net_tmp e->prop->COMMENT=="ADD-VIA-9")
				if(tmp != nil && length(tmp)<9 then
					fprintf(Out_File "%L %L\n" sub net )
				)
			)
		)
	)
	close(Out_File)	

)

;##################################################################### 
axlCmdRegister("find_pin_cutoff" `find_pin_cutoff)
(defun find_pin_cutoff ()
	keepout_shape_list =  setof(e axlDBGetShapes("ROUTE KEEPOUT") e->layer =="ROUTE KEEPOUT/MP32" && length(e->segments)==1 )
	name_file_list = nil
	foreach(item parseString(axlClipboardGetText() "\n")
		item = axlStringRemoveSpaces(item)
		row = item
		if(index(item "\t") != nil then
			row  = car(parseString(item "\t"))
			
		)	
		name_file_list	= cons(row name_file_list)	
	)
	bga = car(setof(e axlDBGetDesign()->symbols e->refdes=="BGA"))
	pins = setof(e bga->pins member(e->net->name name_file_list) != nil)
	xy_tmp = nil
	Out_File = outfile("./find_pin_cutoff.log" "w")
	
	foreach(item keepout_shape_list
		foreach(item2 item->segments
			if(item2->xy != nil then
				xy_tmp = cons(item2->xy xy_tmp)
				;fprintf(Out_File "aa%L\n" item2->xy)
			)
		)
	)
	foreach(pin pins 	
		
		if(member(pin->xy xy_tmp)== nil then
			fprintf(Out_File "%L\n" pin->xy)
		)
	)
	close(Out_File)	

)
;##################################################################### 
axlCmdRegister("counter_via_follow_pin" `counter_via_follow_pin)
(defun counter_via_follow_pin ()
	net_list = nil
	foreach(row parseString(axlClipboardGetText() "\n")
		row = axlStringRemoveSpaces(row)
		net_list = cons(upperCase(row) net_list)			
	)
	nets = axlSelectByName("NET" unique(net_list))
	Out_File = outfile("./counter_via_follow_pin.log" "w")
	foreach(net nets
		pin_list = setof(e axlPinsOfNet(net 'pin) e->parent->refdes =="BGA")
		thick_via_list = nil
		foreach(pin pin_list
			foreach(via axlDBGetConnect(pin t)
				st_via = via->startEnd
				thick_via_list = cons(list(axlViaZLength(car(st_via) cadr(st_via)) st_via) thick_via_list)
			)		
		)
		if(thick_via_list != nil then
			thick_max = cadr(lastelem(sortcar(thick_via_list 'lessp)))
			via_list = nil
			foreach(branch net->branches
				foreach(b setof(e branch->children (e->objType =="via" && index(e->name "TOP") == nil ))
					via_list =  cons(b via_list)
				)
			)
			vp_up = cadr(member(cadr( parseString(car(thick_max) "/")) axlSubclassRoute()))
			vp_down = cadr(member(cadr( parseString(cadr(thick_max) "/")) reverse(axlSubclassRoute())))
			sub_list = member(vp_up axlSubclassRoute(?field 'isEtch ?value nil))
			sub_list = reverse(member(vp_down reverse(sub_list)))
			OK = t
			foreach(sub sub_list
				via_counter = setof(e via_list axlPadOnLayer(e sub)== t)
				if(length(pin_list)*4 != length(via_counter) then
					OK = nil
					printf("%-15s %-15s %d-%d\n" sub net->name length(pin_list)*4  length(via_counter))
					fprintf(Out_File "%-15s %-15s %d-%d\n" sub net->name length(pin_list)*4  length(via_counter))
				)
			)
			if(OK == t then
				fprintf(Out_File "%s------------OK----\n" net->name)
			)
			fprintf(Out_File "----------------------\n" )
		)
	)
	close(Out_File)

)
;##################################################################### 
axlCmdRegister("uni_via7" `uni_via7)
(defun uni_via7 ()
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	while(axlSelect(?prompt "Please select an element...") 
		pitch = 200.0
		Input_List = nil
		Input_List = axlGetSelSet()
		axlClearSelSet()
		foreach(via Input_List
		;foreach(via setof(e Input_List e->prop->COMMENT != nil ) 
			poly_via = car(axlPolyFromDB(via ?layer car(via->startEnd) ?endCapType 'ROUND ?padType 'REGULAR))
			Item_poly_Expand = car(axlPolyExpand(poly_via 400.0 'NONE))
			get_db_list = axlGetSelSet(axlAddSelectBox(Item_poly_Expand->bBox))
			get_db_all = setof(e get_db_list e != via && axlDistance(via->xy e->xy) < pitch )
			if(get_db_all != nil then
				tmp = nil
				foreach(via2 get_db_all
					tmp = cons(axlDistance(via->xy via2->xy) tmp)
				)
				
				tmp = sort(tmp 'lessp)
				min_dis = pitch - car(tmp)
				min_dis = min_dis+1.0
				;println(min_dis)
				tmp_line = axlMXYAdd(via->xy 0 : min_dis )
				ro_point = tmp_line
				loop = 0.0
				while(loop <360.0
					ro_point = axlGeoRotatePt(loop ,ro_point, via->xy)
					tmp2 = setof(e get_db_list e != via && axlDistance(ro_point e->xy) < pitch)
					if(tmp2 == nil then
						loop = 360.0
						println("changed")
						axlTransformObject(via ?move axlMXYSub( ro_point via->xy) )
					)
					;println(loop)
					;println(tmp2~>xy)
					loop= loop+15.0
				)
			)
			
		)
	)

)
;##################################################################### 
axlCmdRegister("copy_comment" `copy_comment)
ip_change_comment = nil
(defun copy_comment ()
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
	axlSingleSelectPoint()
	ori_via = car(axlGetSelSet())
	if(ori_via->prop->COMMENT != nil then
		mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
		axlUIPopupSet( mypopup)
		while(axlSelect(?prompt "Please select an element...") 
			Input_List = nil
			Input_List = axlGetSelSet()
			axlClearSelSet()
			foreach(via setof(e Input_List e->prop->COMMENT != nil ) 
				axlDBAddProp(via list( "Comment" ori_via->prop->COMMENT))
				axlCustomColorObject(via axlIsCustomColored(ori_via))
				
			)
		)
	
	)
		

)
;##################################################################### 
axlCmdRegister("change_comment" `change_comment)
ip_change_comment = nil
(defun change_comment ()
	if(ip_change_comment != nil then
		ip_change_comment2 = axlUIPrompt("Enter comment :" ip_change_comment )	
	else
		ip_change_comment2 = axlUIPrompt("Enter comment :"  )	
	)
	
	if(ip_change_comment2 == nil || ip_change_comment2=="" then
	
	else
		ip_change_comment = ip_change_comment2
		axlClearSelSet()
		axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
		mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
		axlUIPopupSet( mypopup)
		while(axlSelect(?prompt "Please select an element...") 
			Input_List = nil
			Input_List = axlGetSelSet()
			axlClearSelSet()
			foreach(via setof(e Input_List e->prop->COMMENT != nil ) 
				axlDBAddProp(via list( "Comment" ip_change_comment))
				axlCustomColorObject(via)
				
			)
		)
	
	)
	

)
;##################################################################### 
axlCmdRegister("move_test" `move_test)
(defun move_test () 
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	view = nil
	while(axlSelect(?prompt "Please select an element...") 
		if( view != nil then axlUIWClose(view))
		Input_List = nil
		Input_List = axlGetSelSet()
		axlClearSelSet()
		if(length(Input_List)>0  then
			foreach(via setof(e Input_List index(e->name "TOP") == nil)
				poly = car(axlPolyFromDB(via, ?endCapType 'ROUND ?layer cadr(via->startEnd) ?padType 'REGULAR))
				Item_poly_Expand = car(axlPolyExpand(poly 100.0 'NONE)) 
				axlClearSelSet()
				axlSetFindFilter(?enabled '(noall SHAPES NAMEFORM) ?onButtons '(noall SHAPES ))
				get_db_all = axlGetSelSet(axlAddSelectBox(Item_poly_Expand->bBox))
				tmp_list = nil
				foreach(shape get_db_all
					shape_ori = axlDBAltOrigin('center shape)
					tmp_list = cons( list(axlDistance(via->xy shape_ori ) shape_ori  )  tmp_list)
				)
				tmp_list = sortcar(tmp_list 'lessp)
				if(setof(e list(axlDBIsFixed(via->net) axlDBIsFixed(via)) e != nil  ) == nil && index(via->name "BOTTOM") == nil  then
					axlTransformObject(via ?move axlMXYSub(cadr(car(tmp_list)) via->xy ) )
				)
				
				;println(tmp_list)

			)
		)
		axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
	)

)

	
;##################################################################### 
axlCmdRegister("cutoff_LGA" `cutoff_LGA)
(defun copy_coa_lga (pin,point1, point2,point3, net_name)
		via_name_custom = sprintf(nil "%s-%s-%s" "VIA75" st_via_layer end_via_layer)
		l_pd1 = axlLoadPadstack(via_name_custom)
		if(l_pd1 != nil then
			container_db_list = nil 
			foreach(net_in content_row_list
				if(car(net_in)== net_name then
					via_net = cadr(net_in)
				)
			)
			via1 = car(axlDBCreateVia(via_name_custom, point1, via_net , nil , nil ))
			via2 = car(axlDBCreateVia(via_name_custom, point2, via_net , nil , nil ))
			via3 = car(axlDBCreateVia(via_name_custom, point3, via_net , nil , nil ))
			if(via_net != nil then
				axlDBAddProp(list(via1 via2 via3) list( "Comment" strcat("cutoff_Via-" via_net )))
			else
				axlDBAddProp(list(via1 via2 via3) list( "Comment" "cutoff_Via-GND"))
			)
			sub_line_list = member(st_via_layer sub_list)
			sub_line_list = reverse(member(end_via_layer  reverse(sub_line_list)))
			foreach(sub cdr(sub_line_list)
				d_actual = gap_via_via - 37.5
				d = axlMXYAdd( 0:d_actual pin->xy) 
				r_path = axlMakeDynamicsPath(list(list(d d 0.0  pin->xy d_actual t) list(d d) ) )
				poly1 = car(axlPolyFromDB(r_path ?layer strcat("ETCH/" sub) ?endCapType 'ROUND))
				new_shape1 = car(axlDBCreateShape( poly1 t strcat("ROUTE KEEPOUT/" sub) via_net nil))
				if(pin->net->diffpair != nil then
					pair_net = car(setof(e pin->net->diffpair->groupMembers (e != pin->net)))
					container_net_list = cons(pair_net container_net_list)
					pin2 = car(setof(e axlPinsOfNet(pair_net 'pins) index(e->parent->refdes sym) != nil))
					dis_pin1 = axlDistance(car(axlExtentLayout()) pin->xy)
					dis_pin2 = axlDistance(car(axlExtentLayout()) pin2->xy)
					set_ori_pin = list(pin pin2)
					if(dis_pin1 > dis_pin2 then
						set_ori_pin = reverse(set_ori_pin)
					)
					if(angle <1 then
						po_1 = axlMXYSub( car(set_ori_pin)->xy d_actual : 0.0)
						po_2 = axlMXYAdd(d_actual : 0.0 cadr(set_ori_pin)->xy)
					else
						po_1 = axlMXYAdd(0.0 : d_actual car(set_ori_pin)->xy  )
						po_2 = axlMXYSub(cadr(set_ori_pin)->xy 0.0 : d_actual )
					)
					new_shape2 = car(axlDBCreateRectangle(list(po_1 po_2) t strcat("ROUTE KEEPOUT/" sub)  nil))
					d = axlMXYAdd( 0:d_actual pin2->xy) 
					r_path = axlMakeDynamicsPath(list(list(d d 0.0  pin2->xy d_actual t) list(d d) ) )
					poly1 = car(axlPolyFromDB(r_path ?layer strcat("ETCH/" sub) ?endCapType 'ROUND))
					new_shape3 = car(axlDBCreateShape( poly1 t strcat("ROUTE KEEPOUT/" sub) via_net nil))
					axlShapeMerge( new_shape2 list(new_shape1 new_shape3)  'quiet)
				)	
			)
			container_db_list = cons(via1 container_db_list)
			container_db_list = cons(via2 container_db_list)
			container_db_list = cons(via3 container_db_list)
			if(pin->net->diffpair != nil then
				pair_net = car(setof(e pin->net->diffpair->groupMembers (e != pin->net)))
				container_net_list = cons(pair_net container_net_list)
				pin2 = car(setof(e axlPinsOfNet(pair_net 'pins) index(e->parent->refdes sym) != nil))
				axlCopyObject(container_db_list, ?angle 180.0 ?origin pin->xy ?move   axlMXYSub(  pin2->xy  pin->xy))
			)
			
		else
			printf("No found  padstack\n")
		)
		
	

)

(defun cutoff_LGA_form ()
	write_form_cutoff_LGA_form = outfile("./cutoff_LGA.form" "w")
    fprintf(write_form_cutoff_LGA_form, "%s\n%s\n%s\n%s\n%s\n\n","FILE_TYPE=FORM_DEFN VERSION=2" "FORM"  "FIXED" "PORT 22 25" "HEADER \"Create cut off via LGA\"")
	fprintf(write_form_cutoff_LGA_form, "%s %L%L , %L%L.\n\n" "POPUP <m_popup3>" "BGA" "BGA" "U1" "U1")
	fprintf(write_form_cutoff_LGA_form, "%s %L%L , %L%L.\n\n" "POPUP <m_popup2>" "1200" "1200" "925" "925")
	fprintf(write_form_cutoff_LGA_form, "%s %L%L , %L%L.\n\n" "POPUP <m_popup1>" "" "" "" "")
	fprintf(write_form_cutoff_LGA_form, "%s %L%L , %L%L.\n\n" "POPUP <m_popup>" "" "" "" "")
	
	;### TAO TILE
	fprintf(write_form_cutoff_LGA_form, "%s\n\n","TILE" )
	;### FROM LAYER
	fprintf(write_form_cutoff_LGA_form, "%s\n%s\n%s\n\n","TEXT \"Via name\"" "FLOC 3 3" "ENDTEXT")	
	fprintf(write_form_cutoff_LGA_form, "%s\n%s\n%s\n%s\n%s\n%s\n\n","FIELD via_name"  "FLOC 13 3"  "STRFILLIN 20 20"  "OPTIONS prettyprint ownerdrawn" "POP \"m_popup\"" "ENDFIELD")	
	
	fprintf(write_form_cutoff_LGA_form, "%s\n%s\n%s\n\n","TEXT \"Via net\"" "FLOC 3 6" "ENDTEXT")	
	fprintf(write_form_cutoff_LGA_form, "%s\n%s\n%s\n%s\n%s\n%s\n\n","FIELD via_net"  "FLOC 13 6"  "ENUMSET 22 "  "OPTIONS prettyprint ownerdrawn" "POP \"m_popup1\"" "ENDFIELD")	
	
	fprintf(write_form_cutoff_LGA_form, "%s\n%s\n%s\n\n","TEXT \"Via pitch\"" "FLOC 3 9" "ENDTEXT")	
	fprintf(write_form_cutoff_LGA_form, "%s\n%s\n%s\n%s\n%s\n%s\n\n","FIELD via_pitch"  "FLOC 13 9"  "STRFILLIN 20 20"  "OPTIONS prettyprint ownerdrawn" "POP \"m_popup2\"" "ENDFIELD")	
	
	fprintf(write_form_cutoff_LGA_form, "%s\n%s\n%s\n\n","TEXT \"Symbol\"" "FLOC 3 12" "ENDTEXT")	
	fprintf(write_form_cutoff_LGA_form, "%s\n%s\n%s\n%s\n%s\n%s\n\n","FIELD sym"  "FLOC 13 12"  "STRFILLIN 20 20"  "OPTIONS prettyprint ownerdrawn" "POP \"m_popup3\"" "ENDFIELD")	
	
	
	;### END TILE 
	fprintf(write_form_cutoff_LGA_form, "%s\n","ENDTILE" )
	;### END FORM
	fprintf(write_form_cutoff_LGA_form, "%s\n", "ENDFORM")  
    close(write_form_cutoff_LGA_form)
)
(defun cutoff_LGA ()
	via_name = nil
	via_pitch = nil
	cutoff_LGA_form()
	cutoff_LGA_form = nil
	via_net_pu = nil
	content_row_list = nil
	sym  = "BGA"
	foreach(row parseString(axlClipboardGetText() "\n")
		row = axlStringRemoveSpaces(row)
		if(index(row "\t") != nil then
			par_row = parseString(row, "\t")
			if(length(par_row)==2 then
				content_row_list = cons(list(upperCase(car(par_row)) cadr(par_row))  content_row_list)
				via_net_pu = cons( list(cadr(par_row) cadr(par_row)) via_net_pu)
			) 
		)	
	)
    axlControlRaise('options)
    cutoff_LGA_form = axlMiniStatusLoad((gensym) "cutoff_LGA.form" 'cutoff_LGA_form_action nil)
	via_name_list = setof(e sort(axlGetAllViaList()~>name 'alphalessp) index(e "TOP") == nil )
	via_name_list = setof(e via_name_list index(e "BOTTOM") == nil )
	via_name_list = setof(e via_name_list index(e "VIA75") != nil )
	via_name_pu = nil
	if(via_net_pu == nil then via_net_pu = list(list("GND" "GND") list("VSS" "VSS")))
	via_net_pu = unique(via_net_pu)
	foreach(via via_name_list
		via_name_pu = cons(list(via via) via_name_pu)
	)
	axlFormBuildPopup(cutoff_LGA_form "via_name" via_name_pu )
	axlFormBuildPopup(cutoff_LGA_form "via_net" via_net_pu )
	axlFormDisplay(cutoff_LGA_form)
	axlClearSelSet()
    axlSetFindFilter(?enabled '(noall  NETS NAMEFORM) ?onButtons '(noall NETS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	view = nil
    while(axlSelect(?prompt "Please select an element...") 
		if( view != nil then axlUIWClose(view))
			Input_List = nil
			Input_List = axlGetSelSet()
			axlClearSelSet()
			if(length(Input_List)>0 && via_name != nil && via_pitch != nil then
				cutoff_LGA2()
			)
	)
)
(defun cutoff_LGA_form_action (cutoff_LGA_form)
	;println(cutoff_LGA_form->??)
	if(cutoff_LGA_form->curField =="via_name" then
		tmp_val = cutoff_LGA_form->curValue
		if(member(tmp_val via_name_list) != nil then
			via_name = tmp_val
		)
		axlFormSetField(cutoff_LGA_form "via_name" via_name)
	)
	if(cutoff_LGA_form->curField =="via_pitch" then
		tmp_val = cutoff_LGA_form->curValue
		if(atoi(tmp_val)>0 then
			via_pitch = atoi(tmp_val)
		)
	)
 )
(defun cutoff_LGA2 ()
	STT =0
	Out_File = outfile("./create_coa.log" "w")
	container_net_list = nil
	via_name = axlFormGetField(cutoff_LGA_form "via_name")
	via_pitch = atoi(axlFormGetField(cutoff_LGA_form "via_pitch"))
	sym = axlFormGetField(cutoff_LGA_form "sym")
	printf("%L %L %L\n" via_name via_pitch sym)
	;gap_via_via = via_pitch/2.0 +37.5
	gap_via_via = via_pitch/2.0
	printf("%L\n" gap_via_via)
	p_via_name = parseString(via_name "-")
	st_via_layer = cadr(p_via_name)
	end_via_layer = caddr(p_via_name)
	line_width = 75.0
	via_net = nil
	sub_list = axlSubclassRoute(?field 'isEtch)
	foreach(net Input_List
		via_net = nil
		net_name = net->name
		if(member(net container_net_list) == nil then
			container_net_list = cons(net container_net_list)
			if(net->diffpair != nil then
				pair_net = car(setof(e net->diffpair->groupMembers (e != net)))
				container_net_list = cons(pair_net container_net_list)
				pin1 = car(setof(e axlPinsOfNet(net 'pins) index(e->parent->refdes sym) != nil))
				pin2 = car(setof(e axlPinsOfNet(pair_net 'pins) index(e->parent->refdes sym ) != nil))
				if(pin1 != nil && pin2 != nil then
					dis_pin_pin = axlDistance(pin1->xy pin2->xy)
					if(dis_pin_pin<1000.1 then
						temp_point1 = axlMXYSub(  pin1->xy list(0.0 gap_via_via) ) 
						angle = abs(axlRadToDeg(_axlAngleBetweenLines(list(temp_point1 pin1->xy), list(pin1->xy pin2->xy))))
						r_angle = (360.0 - angle) +180.0
						ro_point_via = axlGeoRotatePt(r_angle temp_point1 pin1->xy)
						if(axlIsPointOnLine(ro_point_via list(pin1->xy pin2->xy) ) == t then
							ro_point_via = axlGeoRotatePt(180.0 ro_point_via pin1->xy)
						)
						ro_point_via2 = axlGeoRotatePt(90.0 ro_point_via pin1->xy)
						ro_point_via3 = axlGeoRotatePt(180.0 ro_point_via2 pin1->xy)
						copy_coa_lga(pin1,ro_point_via, ro_point_via2,ro_point_via3,net_name)
					else
						printf("distance over %s\n" net_name)
					)
				else
					printf("not found pin pair %s\n" net_name )
					
				)	
			)	
		)
	)
	close(Out_File)
)

;##################################################################### 
axlCmdRegister("via_cutoff_chk" `via_cutoff_chk)
(defun via_cutoff_chk () 
	keepout_shape_list = setof(e axlDBGetShapes("ROUTE KEEPOUT") e->layer =="ROUTE KEEPOUT/MP32" && length(e->segments)==4 || e->layer =="ROUTE KEEPOUT/MP32" && length(e->segments)==1 )
	Out_File = outfile("./via_cutoff_chk.log" "w")
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	axlSingleSelectName("PROPERTY" list(list("COMMENT") "auto_cutoff_via"))
	via_list = axlGetSelSet()
	foreach(shape keepout_shape_list
		shape_poly = car(axlPolyFromDB(shape))
		Item_Expand = car(axlPolyExpand(shape_poly 75.0 'NONE))
		;via_list2 = setof(e via_list  axlGeoPointInShape(e->xy shape_poly nil) == t )
		via_list2 = setof(e via_list  axlGeoPointInShape(e->xy Item_Expand nil) == t )
		if(length(via_list2) <2 then
			
			fprintf(Out_File "%-10s %L\n" strcat("ETCH/" cadr(parseString(shape->layer "/") )) axlDBAltOrigin('center shape))
		)
		
	)
	close(Out_File)
	 view = axlUIViewFileCreate("via_cutoff_chk.log", " " nil)
) 
;##################################################################### 
ip_hl_via_guard = nil
axlCmdRegister("hl_via_guard2" `hl_via_guard2) 
(defun hl_via_guard2 ()
	if(ip_hl_via_guard != nil then
		ip_hl_via_guard2 = axlUIPrompt("Enter a Via Name :" ip_hl_via_guard )	
	else
		ip_hl_via_guard2 = axlUIPrompt("Enter a Via Name :" ip_hl_via_guard )	
	)
	if(ip_hl_via_guard2 != nil then
		ip_hl_via_guard = ip_hl_via_guard2
		;axlClearSelSet()
		;axlSetFindFilter(?enabled '(noall VIAS  NAMEFORM) ?onButtons '(noall VIAS))
		;axlAddSelectAll()
		net_list = axlDBGetDesign()->nets
		;net_list = setof(e axlDBGetDesign()->nets index(e->name "SRD_AGND") != nil )
		via_list = nil
		sub_list = nil
		foreach(sub   axlSubclassRoute()
			if(axlIsVisibleLayer(strcat("VIA CLASS/" sub)) == t then
				sub_list = cons(strcat("VIA CLASS/" sub) sub_list)
			)
		)
		foreach(net net_list
			foreach(branch net~>branches
				via_list_tmp =  setof(e branch->children (e->objType == "via"))
				foreach(via via_list_tmp
					if(car(setof(e sub_list axlPadOnLayer(via e) == t)) != nil then
						via_list = cons(via via_list)
					)
					
				)
           
			)
		)
		if(index(ip_hl_via_guard2 " ") != nil then
			sel = setof(e via_list e->prop->COMMENT == car(parseString(ip_hl_via_guard " "))|| e->prop->DYN_THERMAL_CON_TYPE == car(parseString(ip_hl_via_guard " ")))
			axlCustomColorObject(sel atoi(cadr(parseString(ip_hl_via_guard " "))))
		else
			sel = setof(e via_list e->prop->COMMENT == ip_hl_via_guard)
			axlCustomColorObject(sel )
		)	
	)
)

;##################################################################### 
axlCmdRegister("hl_via_guard" `hl_via_guard) 
input_hl_via_guard = nil
(defun hl_via_guard () 
	;ori_visible = axlVisibleGet()
	println(input_hl_via_guard)
	if(input_hl_via_guard != nil then
		input_hl_via_guard2 = axlUIPrompt("Enter parameters :" input_hl_via_guard )
	else
		input_hl_via_guard2 = axlUIPrompt("Enter parameters :" "" )
	)
	
	if(input_hl_via_guard2 != nil then
		input_hl_via_guard = input_hl_via_guard2
		axlClearSelSet()
		axlSetFindFilter(?enabled '(noall VIAS  NAMEFORM) ?onButtons '(noall VIAS))
		axlAddSelectAll()
		;sel = setof(e axlGetSelSet() e->prop->DYN_THERMAL_CON_TYPE =="FULL_CONTACT" &&  axlPadOnLayer(e strcat("ETCH/" input_hl_via_guard2  ) t) == t)
		sel = setof(e axlGetSelSet()  axlPadOnLayer(e strcat("ETCH/" input_hl_via_guard2  ) t) == t)
		axlCustomColorObject(sel )
		;axlVisibleSet(ori_visible)
	)
	
)
;##################################################################### 
axlCmdRegister("chk_via_VDD_PB_NC1760" `chk_via_VDD_PB_NC1760) 
(defun chk_via_VDD_PB_NC1760 ()
	view = nil
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall PINS  NAMEFORM) ?onButtons '(noall PINS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet(mypopup)	
	while(axlSelect(?prompt "Please select an element...")
		if( view != nil then axlUIWClose(view))
		sel_list = nil
		sel_list = axlGetSelSet()
		axlClearSelSet()
		STT =0
		Out_File = outfile("./chk_via_VDD_PB_NC1760.log" "w")
		foreach(pin_item setof(e sel_list e->net->name =="DVDD_0P75")
			axlClearSelSet()
			axlSetFindFilter(?enabled '(noall VIAS  NAMEFORM) ?onButtons '(noall VIAS))
			axlAddSelectBox(pin_item->bBox)
			se_via_list = setof(e axlGetSelSet() ( e->net->name == "VSS" && abs(axlDistance(e->xy pin_item->xy) - 212.1320) <0.01) )
			if(length(se_via_list) != 4 then
				fprintf(Out_File "%L\n" pin_item->xy)
			)
			

									  
		)
		close(Out_File)
		axlSetFindFilter(?enabled '(noall PINS  NAMEFORM) ?onButtons '(noall PINS))
		printf("Total: %n Pins Unconnect\n" STT)
		if(STT == 0 then
		 axlUIConfirm( "Finished!")
		 else
		 view = axlUIViewFileCreate("chk_via_VDD_PB_NC1760.log", " " nil)
		)
	)
	
)
;##################################################################### 
axlCmdRegister("chk_via_VSS_PB_NC1760" `chk_via_VSS_PB_NC1760) 
(defun chk_via_VSS_PB_NC1760 ()
	etch_shape_list = setof(e axlDBGetShapes("ETCH") (e->net->name =="VSS"&&  e->symbolEtch== nil && e->layer =="ETCH/MP17" && e->shapeBoundary == nil) )
	Out_File = outfile("./chk_via_VSS_PB-NC1760.log" "w")
	row_container_list = nil
	foreach(shape etch_shape_list
		conn_shape_list= setof(e axlDBGetConnect(shape t) e->objType == "via")
		chk_via_btm = setof(e conn_shape_list index(e->name "BOTTOM") != nil)
		if(length(conn_shape_list) != 9 then
			row = sprintf(nil "NG Via Missing Quality %-10s %L\n" shape->layer axlDBAltOrigin('center shape))
			row_container_list = cons(row row_container_list)
		else
			if(length(chk_via_btm) != 5 then
				row = sprintf(nil "NG Via Bottom %-10s %L\n" shape->layer axlDBAltOrigin('center shape))
				row_container_list = cons(row row_container_list)
			else
				foreach(via setof(e conn_shape_list index(e->name "BOTTOM") == nil)
					chk1 = axlPadOnLayer(via "MP4" t)
					chk2 = axlPadOnLayer(via "MP17" t)
					if(chk1 == nil || chk2 == nil then
						row = sprintf(nil "NG Via from Upper %-10s %-10s %L\n" shape->layer via->name axlDBAltOrigin('center shape))
						row_container_list = cons(row row_container_list)
					)
				)
				
			)
		)	
	)
	foreach(row sort( row_container_list 'alphalessp)
		fprintf(Out_File "%s" row)
	)
	close(Out_File)
	axlUIViewFileCreate("chk_via_VSS_PB-NC1760.log", "Show Element To Check Pins" nil)
	
)

;##################################################################### 
axlCmdRegister("cut_via_layer" `cut_via_layer) 
(defun cut_via_layer () 
	view = nil
	axlClearSelSet()
    axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	 while(axlSelect(?prompt "Please select an element...")
		if( view != nil then axlUIWClose(view) )
		STT =0
		sub_info = car(setof(e axlVisibleGet() member("ETCH" e) != nil ))->subclassinfo
		sub_info_via = car(setof(e axlVisibleGet() member("VIA CLASS" e) != nil ))->subclassinfo
		sub_vis = setof(e sub_info cadr(e) == t)
		sub_vis_via = setof(e sub_info_via cadr(e) == t)
		if(length(sub_vis)>1  || length(sub_vis_via)>1 then
		  axlUIConfirm( "Visible layer was invalid\n Try again!" 'error )                
		else
			;if(length(sub_vis)>1 then 
			sub_vis = caar(reverse(sub_vis))
			STT =0
			axlUIWCloseAll()
			Input_List = nil
			Input_List =  axlGetSelSet()
			axlClearSelSet()
			Out_File = outfile("./cut_via_layer.log" "w")
			printf("%L\n" Input_List)
			check_fix = setof(e Input_List axlDBIsFixed(e->net) == nil)
			if(length(check_fix)==0 then 
				axlUIConfirm( "No found")
			else
				row_container_list = nil
				foreach(via check_fix
					if(axlDBIsFixed(via) == nil then
						via_name = via->name
						
						if(index(via->name "-") != nil then
							par_via_name = parseString(via_name "-")
							st_end_via = via->startEnd
							par_str_st = cadr(parseString(car(st_end_via) "/"))
							par_str_end = cadr(parseString(cadr(st_end_via) "/"))
							start_via1 = cadr(member(sub_vis reverse(axlSubclassRoute(?field 'isEtch))))
							start_via2 = cadr(member(sub_vis axlSubclassRoute(?field 'isEtch)))
							via_name1 = sprintf(nil "%s-%s-%s" car(par_via_name) par_str_st start_via1)
							via_name2 = sprintf(nil "%s-%s-%s" car(par_via_name) start_via2 par_str_end)
							load_via_name1 = axlLoadPadstack(via_name1)
							load_via_name2 = axlLoadPadstack(via_name2)
							via_NG = nil
							if(load_via_name1 != nil && load_via_name2 != nil then
								chk_via1 = unique(load_via_name1->startEnd)
								chk_via2 = unique(load_via_name2->startEnd)
								
								if(length(chk_via1)==2 && length(chk_via2)==2 then 
									via_comment = nil
									if(via->prop->COMMENT != nil then
										if(via->net->name == nil then 
											via_comment= sprintf(nil "%s %s" via->prop->COMMENT "")
										else
											via_comment= sprintf(nil "%s %s" via->prop->COMMENT via->net->name)
										)
										
									else
										via_comment=via->net->name
									)
									axlDBAddProp(via list( "Comment" via_comment))
									copy_item = car(axlCopyObject(via ?angle 0.0))
									axlReplacePadstack(via via_name1)
									axlReplacePadstack(copy_item via_name2)
									
									if( copy_item->prop != nil then
										axlCopyProperties(via, copy_item)
									)
									;fprintf(Out_File "%-10s %-10s %-10s %L\n" via_name via_name1 via_name2  via->xy)
								else
									via_NG = t
								)
							else
								via_NG = t
							)
							if(via_NG == t then
								row_container_list = cons(sprintf(nil "Via was invalid  %-10s %L\n" via_name via->xy) row_container_list)
							else
								row_container_list = cons(sprintf(nil "Via was change successfully  %-10s %-10s %-10s %L\n" via_name via_name1 via_name2  via->xy) row_container_list)
							) 
						else
							
							row_container_list = cons(sprintf(nil "Via was invalid %-10s %L\n" via->name  via->xy) row_container_list)
						)
						
					else
						row_container_list = cons(sprintf(nil "Via was fixed %-10s %L\n" via->name  via->xy) row_container_list)
					)
					STT++
				)
				row_container_list = sort(row_container_list 'alphalessp)
				foreach(row row_container_list
					fprintf(Out_File "%s" row)
				)
				
			)
				close(Out_File)
				if(STT>0 then
					axlUIViewFileCreate("cut_via_layer.log", "Show Element To Check Pins" nil)
				)
			)
			;) 
	)
)

;##################################################################### 
axlCmdRegister("split_via" `split_via)
(defun split_via () 
	if(axlVersion('version) >= 17.2 then
	spacing_via_shape = 96.02
	view = nil
	subs = nil
	foreach(sub axlSubclassRoute(?field 'isEtch) 
		subs  = cons(strcat("ETCH/" sub) subs )
	)
	subs = reverse(subs)
	axlClearSelSet()
    axlSetFindFilter(?enabled '(noall VIAS SHAPES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	 while(axlSelect(?prompt "Please select an element...")
        STT =0
        axlUIWCloseAll()
		ori_visible = axlVisibleGet()
		if( view != nil then axlUIWClose(view) )
        Input_List = nil
        Input_List =  axlGetSelSet()
		Out_File = outfile("./split_via.log" "w")
		pad_onlayer_list = nil
		foreach(sub subs
			get_pad_layer = setof(e Input_List axlPadOnLayer(e sub t) == t)
			if(get_pad_layer != nil then
				pad_onlayer_list = cons(list(sub get_pad_layer) pad_onlayer_list)
			)
			;printf("%s %L\n" sub get_pad_layer)	
		)
		;printf("%L\n" pad_onlayer_list)	
		data_via_list = nil
		foreach(sub subs
			foreach(pad_layer setof(e pad_onlayer_list car(e) == sub )
				layer = car(pad_layer)
				foreach(via cadr(pad_layer)
					poly_via = car(axlPolyFromDB(via ?endCapType 'ROUND ?layer layer ?padType 'REGULAR))
					axlClearSelSet()
					axlSetFindFilter(?enabled '(noall VIAS SHAPES CLINES NAMEFORM) ?onButtons '(noall VIAS SHAPES CLINES ))
					;get_db_all = axlGetSelSet(axlAddSelectBox(poly_via->bBox))
					layer_visi = cadr(parseString(sub "/"))
					axlVisibleDesign(nil)
					axlVisibleLayer(strcat("VIA CLASS/" layer_visi) t)
					axlVisibleLayer(strcat("PIN/" layer_visi) t)
					axlVisibleLayer(strcat("ETCH/" layer_visi) t)
					axlVisibleLayer(strcat("DRC ERROR CLASS/" layer_visi) t)
					axlVisibleLayer(strcat("PLAN/" layer_visi) t)
					size_via = (xCoord(cadr(poly_via->bBox)) - xCoord(car(poly_via->bBox)))/2
					Item_poly_Expand = car(axlPolyExpand(poly_via (spacing_via_shape - size_via) 'NONE))
					;create_shape = axlDBCreateShape(Item_poly_Expand t  "ANTI ETCH/ALL" nil )
					get_db_all = setof(e axlGetSelSet(axlAddSelectPoly(Item_poly_Expand)) e != via)
					;get_db_all = setof(e axlGetSelSet( axlAddSelectBox(poly_via->bBox) ) e != via)
					if(get_db_all != nil then
						data_via_list = cons(list(via sub car(get_db_all)->net->name) data_via_list)
					)
				
				)
				
			)
		)
		data_via_list = reverse(data_via_list)
		foreach(via Input_List
			data_list = setof(e data_via_list car(e) == via)
			via_layer_list = nil
			foreach(sub subs
				foreach(data data_list
					via_layer_list = cons(list( cadr(data)  caddr(data) ) via_layer_list)
				)
			)
			via_layer_list = unique(reverse(via_layer_list))
			if(length(via_layer_list)>1 then
				fprintf(Out_File "%-10s %-10s %L\n" via->name via->net->name via->xy)
				fprintf(Out_File "%L\n" via_layer_list)
			)
			
			
		
		)
		;fprintf(Out_File "%L\n" data_via_list)
		close(Out_File)
		axlVisibleSet(ori_visible)
		axlSetFindFilter(?enabled '(noall VIAS SHAPES NAMEFORM) ?onButtons '(noall VIAS))
		axlUIViewFileCreate("split_via.log", "Show Element To Check Pins" nil)
		   
		)    
	)
)
;##################################################################### 
axlCmdRegister("join_via" `join_via)
(defun join_via ()
	axlClearSelSet()
    axlSetFindFilter(?enabled '(noall VIAS SHAPES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	 while(axlSelect(?prompt "Please select an element...")
        STT =0
        axlUIWCloseAll()
        Input_List = nil
        Input_List =  axlGetSelSet()
		Out_File = outfile("./join_via.log" "w")
		checked_list = nil
		foreach(via Input_List
			if(member(via checked_list) == nil then 
				checked_list = cons(via checked_list)
				via_xy = via->xy
				sel_via_xy = setof(e Input_List e!= via && axlDistance(e->xy via->xy)<0.011 )
				
				if(sel_via_xy != nil then
					via_same_xy_list = nil
					via_same_xy_list = cons(car(via->startEnd) via_same_xy_list)
					via_same_xy_list = cons(cadr(via->startEnd) via_same_xy_list)
					foreach(via2 sel_via_xy
						checked_list = cons(via2 checked_list)
						via_same_xy_list = cons(car(via2->startEnd) via_same_xy_list)
						via_same_xy_list = cons(cadr(via2->startEnd) via_same_xy_list)
						; if(index(via->name "TOP" ) != nil  then
							; if(via2->net != via->net &&  index(via2->name "BOTTOM" ) == nil  then
								; fprintf(Out_File "%-10s %-10s %L\n" via->name via2->name via_xy)
							; )
						; )
					)
					tmp_list = nil
					foreach(sub axlSubclassRoute(?field 'isEtch)
						if(member(strcat("ETCH/" sub) via_same_xy_list) != nil then
							tmp_list = cons(sub tmp_list)
						)
					)
					tmp_list = reverse(tmp_list)
					pre_name_via = car(parseString(via->name "-"))
					name_via_tmp = sprintf(nil "%s-%s-%s" pre_name_via car(tmp_list) lastelem(tmp_list))
					if(pre_name_via != name_via_tmp then
						sel_via_xy2 = setof(e Input_List e->xy == via->xy )
						axlReplacePadstack(sel_via_xy2 name_via_tmp )
					)
					
					fprintf(Out_File "%-10s %L %L\n" via->name via_xy name_via_tmp)
					;axlDBAddProp( via list( "Comment" "overlap_0.01"))
				)
				
			)
					
		)
		close(Out_File)
		axlSetFindFilter(?enabled '(noall VIAS SHAPES NAMEFORM) ?onButtons '(noall VIAS SHAPES))
		axlUIViewFileCreate("join_via.log", "Show Element To Check Pins" nil)
		   
		)    
)
;##################################################################### 
axlCmdRegister("change_netvia2" `change_netvia2)
(defun change_netvia2 ()
	axlClearSelSet()
    axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	axlSingleSelectPoint()
	via_ori_net = axlGetSelSet()
	if(via_ori_net != nil then
	println(car(via_ori_net)->name)
	println(car(via_ori_net)->net->name)
	axlClearSelSet()
    axlSetFindFilter(?enabled '(noall VIAS SHAPES NAMEFORM) ?onButtons '(noall VIAS SHAPES))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	 while(axlSelect(?prompt "Please select an element...")
        STT =0
        axlUIWCloseAll()
        Input_List = nil
        Input_List =  axlGetSelSet()
		Out_File = outfile("./change_net_via2.log" "w")
		foreach(dbid Input_List
			if(dbid->objType=="shape" then
				shape_poly = car(axlPolyFromDB(dbid ->shapeBoundary))
				;axlDBCreateShape(shape_poly  t "ANTI ETCH/Mp19" nil)
				axlClearSelSet()
				axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS ))
				axlAddSelectBox(shape_poly->bBox)
				vias = setof(e axlGetSelSet() axlGeoPointInShape(e->xy shape_poly nil) == t )
				foreach(via vias
					if(via->net->name != car(via_ori_net)->net->name then
						axlChangeNet(via car(via_ori_net)->net->name)
						fprintf(Out_File "%L %L\n" via->name via->xy)
					;else
						;fprintf(Out_File "%L %L\n" via->net->name car(via_ori_net)->net->name)
					)
					
				)
				
			else
				via = dbid
				if(via->net->name != car(via_ori_net)->net->name then
						axlChangeNet(via car(via_ori_net)->net->name)
						fprintf(Out_File "%L %L\n" via->name via->xy)
					)
				
			)	
		)
		close(Out_File)
		axlSetFindFilter(?enabled '(noall VIAS SHAPES NAMEFORM) ?onButtons '(noall VIAS SHAPES))
		axlUIViewFileCreate("change_net_via2.log", "Show Element To Check Pins" nil)
		   
		)    
	)
    
)

;##################################################################### 
axlCmdRegister("dich999" `dich999)
(defun dich999 ()
    sub_info = car(setof(e axlVisibleGet() member("DRC ERROR CLASS" e) != nil ))->subclassinfo
    sub_vis = setof(e sub_info cadr(e) == t)
    printf("DRC length : %L\n" length( sub_vis))
    if(length( sub_vis)>1 then
    axlUIConfirm( "Visible layer DRCs was invalid\n Try again!" 'error )                
    else
    ;input_string = axlUIPrompt("Enter parameters :" "200.0" )
    axlClearSelSet()
    sub_list = axlSubclassRoute(?field 'isEtch)
    cut_via1 = reverse(member("MP6" reverse(sub_list)))
    axlSetFindFilter(?enabled '(noall DRCS NAMEFORM) ?onButtons '(noall DRCS))
    mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
    axlUIPopupSet( mypopup)
    while(axlSelect(?prompt "Please select an element...")
        STT =0
        axlUIWCloseAll()
        Input_List = nil
        Input_List =  axlGetSelSet()
        Out_File = outfile("./dich999.log" "w")
        fprintf(Out_File "---------------------\n" )
        foreach(drc Input_List
            axlClearSelSet()
            axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
            p1 = axlMXYSub(drc->xy 1 : 1.0)
            p2 = axlMXYAdd(drc->xy 1.0 : 1.0)
            axlSingleSelectBox(list(p1 p2))
            ori_via =  axlGetSelSet()
            if(ori_via != nil then
                ori_via = car(ori_via)
                
                pad_box = axlPolyFromDB(ori_via ?endCapType 'ROUND ?layer strcat("ETCH/" caar(sub_vis)) ?padType 'REGULAR)
                if( pad_box != nil then
                    sub_value = atof(drc->expected)
                    sub_value2 = sub_value + 0.1
                    sub_value3 = sub_value - 0.1
                    axlClearSelSet()
                    axlSetFindFilter(?enabled '(noall VIAS) ?onButtons '(noall VIAS))
                    Item_poly_Expand = car(axlPolyExpand(car(pad_box) sub_value 'NONE)) 
                    ;axlDBCreateShape(Item_poly_Expand t strcat("ANTI ETCH/" caar(sub_vis)) nil)
                    axlSingleSelectBox(Item_poly_Expand->bBox)
                    ori_via2 =  setof(e axlGetSelSet() e != ori_via)
                    filter_list  = nil
                    foreach(via2 ori_via2
                       air_gap_2via = axlAirGap(via2 ori_via strcat("ETCH/" caar(sub_vis))) 
                       if(air_gap_2via != nil then
                            air_v = lastelem(air_gap_2via)
                            if(air_v < sub_value2 then
                                if(air_v > sub_value3 then
                                    filter_list = cons(list(air_v via2) filter_list)
                                )
                            )
                            
                       )
                    )
                    filter_list = sortcar(filter_list 'lessp)
                    if(length(filter_list) == 2  then
                        item_move_list = list(list(0 0.01) list(0 -0.01) list(0.01 0) list(-0.01 0))
                        point_move = nil
                        temp_list = nil
                        foreach(item item_move_list
                            chk1 = axlMXYSub(  ori_via->xy item)
                            chk2 = axlDistance(cadr(lastelem(filter_list))->xy chk1)
                            temp_list = cons(list(chk2 chk1) temp_list)
                        )
                        temp_list = sortcar(temp_list 'lessp)
                        axlTransformObject(ori_via ?move axlMXYSub(  cadr(car(temp_list)) ori_via->xy ))
                        fprintf(Out_File "A %L %L\n" ori_via->xy  cadr(car(temp_list)))
                        
                    else
                        printf("No Found")
                        fprintf(Out_File "B %L %L\n" ori_via->xy ori_via2)
                    )
                    
                )  
            )   
        )
        axlSetFindFilter(?enabled '(noall DRCS NAMEFORM) ?onButtons '(noall DRCS))
        close(Out_File)
        ;axlUIViewFileCreate("dich999.log", "Show Element To Check Pins" nil)
    )
   
    )  
        
)

;##################################################################### 
axlCmdRegister("rlayer" `rlayer)
procedure( rlayer()
    let( (xs name ds ret)
        pair_layer_name = axlUIPrompt( "Enter pair layer" "VP MP" )
        if(pair_layer_name != nil then
            pair_layer_name = parseString(pair_layer_name " ")
            sub_list = setof(e reverse(axlSubclassRoute()) e !="BOTTOM")
            counter_layer = length(sub_list)
            foreach(old_sub sub_list
                if(old_sub !="TOP" then
                    if(mod(counter_layer 2)==0 then
                        die = sprintf(nil "%s%d" car(pair_layer_name) counter_layer/2 )
                        if(die != old_sub  then
                            printf("Chan old_sub : %s new_sub : %s\n" old_sub die)
                            xs = axlXSectionGet(nil old_sub)
                            when( xs
                                ds = axlXSectionModify(
                                ?name die
                                )
                                ret = axlXSectionSet(xs ds)
                            )
                            xs
                        else
                            printf("Chan Subclass name already exists in cross-section\n")
                        
                        ) 
                    else
                        con = sprintf(nil "%s%d"  cadr(pair_layer_name) counter_layer/2 +1)
                        if(con != old_sub  then
                            printf("le old_sub : %s new_sub : %s\n" old_sub con)
                            xs = axlXSectionGet(nil old_sub)
                            when( xs
                                ds = axlXSectionModify(
                                ?name con
                                )
                                ret = axlXSectionSet(xs ds)
                            )
                            xs
                        else
                            printf("Le Subclass name already exists in cross-section\n")
                        
                        )
                        
                    )
                )
                counter_layer = counter_layer-1
                
            )
        else
        )  
))


;##################################################################### 
axlCmdRegister("chk2_KBK10_15" `chk2_KBK10_15)
(defun chk2_KBK10_15 ()
    axlClearSelSet()
    sub_list = axlSubclassRoute(?field 'isEtch)
    cut_via1 = reverse(member("MP8" reverse(sub_list)))
    axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
    mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
    axlUIPopupSet( mypopup)
    while(axlSelect(?prompt "Please select an element...")
        STT =0
        axlUIWCloseAll()
        Input_List = nil
        Input_List =  axlGetSelSet()
        Out_File = outfile("./chk2_KBK10_15.log" "w")
        fprintf(Out_File "---------------------\n" )
        getdata_list = nil
        xy_list = nil
        foreach(via setof(e Input_List e->net->name !="")
            xy_list = cons(via->xy xy_list)
            getdata_list = cons(list(via->xy via)  getdata_list)
               
        )
        foreach(xy unique(xy_list)
            chk1_list = setof(e getdata_list car(e)==xy)
            ;if(length(chk1_list)<2 then
                chk1_list= car(chk1_list)
                via  = cadr(chk1_list)
                st_via = cadr(parseString(car(via->startEnd) "/" ))
                end_via = cadr(parseString(cadr(via->startEnd ) "/" ))
                if(member(st_via cut_via1) != nil && member(end_via cut_via1) == nil  then
                    via_name1 = ""
                    via_name11 = ""
                    via_name12 = ""
                    if(st_via != "MP8" then
                        via_name1 = sprintf(nil "VIA100-%s-%s" st_via "MP8")
                    )
                    
                    
                    if(end_via != "MP9" && end_via !="MP8" then
                        via_name12 = sprintf(nil "VIA100-%s-%s" "MP9" end_via)
                    )
                    if(via_name1 != "" &&  end_via !="MP8" then
                        via_name11 = "VIA170-MP8-MP9"
                        fprintf(Out_File "%s\n" via->name)
                        l_pd1 = axlLoadPadstack(via_name1)
                        l_pd2 = axlLoadPadstack(via_name11)
                        l_pd3 = axlLoadPadstack(via_name12)
                        if(l_pd1 != nil && l_pd2 != nil then 
                            fprintf(Out_File "%-10s %-10s %-10s %-15L\n" via_name1 via_name11 via_name12 xy)
                        else
                            fprintf(Out_File "NG %-10s %-10s %-10s %-15L\n" via_name1 via_name11 via_name12 xy)
                        )
                       net_id = via->net->name
                        via = axlReplacePadstack(via via_name1)
                           
                        via2 = axlCopyObject(via ?angle 0.0)
                        via2 = axlReplacePadstack(via2 via_name11)
                       axlChangeNet(car(via) net_id)
                       axlChangeNet(car(via2) net_id)
                       if(l_pd3 != nil then
                           via3 = axlCopyObject(via2 ?angle 0.0)
                            new_via3 = axlReplacePadstack(via3 via_name12)
                            axlChangeNet(car(new_via3) net_id)
                            
                        )
                    )             
                )
            ;else
                ;chk2_list = setof(e chk1_list cadr(e)->name =="VIA170-MP8-MP9" )
                ;if(chk2_list == nil then 
                ;    fprintf(Out_File "%-L %-L\n" chk2_list xy) 
                ;)  
            ;)
        )
        close(Out_File)
        axlUIViewFileCreate("chk2_KBK10_15.log", "Show Element To Check Pins" nil)
    )
   
       
        
)
;##################################################################### 
axlCmdRegister("cut_padstack" `cut_padstack)
(defun cut_padstack ()
    sub_class_visi_list = nil
        foreach(item setof(e axlVisibleGet() e->class=="DRC ERROR CLASS")  foreach(e item->subclassinfo if(cadr(e) == t && member(car(e) axlSubclassRoute(?field 'isEtch)) != nil then sub_class_visi_list = cons(car(e) sub_class_visi_list))))
            sub_class_visi_list = unique(sub_class_visi_list)
            if(length(sub_class_visi_list)==1 then
            sub =   car(sub_class_visi_list)
            sub_list = axlSubclassRoute(?field 'isEtch)
            axlClearSelSet()
            axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
            mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
            axlUIPopupSet( mypopup)
            while(axlSelect(?prompt "Please select an element...")
                STT =0
                axlUIWCloseAll()
                Input_List = nil
                Input_List =  axlGetSelSet()
                Out_File = outfile("./cut_padstack.log" "w")
                foreach(via setof(e Input_List e->net->name =="VSS" && e->prop->COMMENT=="Coaxial-VIA" )
                    ;polyList = car(axlPolyFromDB(item ?layer layer ?endCapType 'SQUARE ?padType 'REGULAR))
                    st_via = cadr(parseString(car(via->startEnd) "/" ))
                    end_via = cadr(parseString(cadr(via->startEnd ) "/" ))
                    st_via_list = member(st_via sub_list)
                    st_via_list = reverse(member(end_via reverse(st_via_list)))
                    cut1 = reverse(cdr(member(sub reverse(st_via_list))))
                    cut2 = cdr(member(sub st_via_list))
                   
                    if( length(cut2)>1 then 
                        via_name1 = sprintf(nil "VIA75-%s-%s" car(cut1) lastelem(cut1))
                        via_name2 = sprintf(nil "VIA75-%s-%s" car(cut2) lastelem(cut2))
                        l_pd1 = axlLoadPadstack(via_name1)
                        l_pd2 = axlLoadPadstack(via_name2)
                        via_net = via->net->name
                        if( l_pd2 != nil then
                            if( length(cut1)==1 then 
                                fprintf(Out_File "%-10s %-10s %L\n" via->name via_name2 via->xy)   
                                new_via1 = axlReplacePadstack(via via_name2)
                                axlDBAssignNet(new_via1 via_net t)
                            else
                                if(l_pd1 != nil then
                                    fprintf(Out_File "%-10s %-10s %-10s %L\n" via->name via_name1 via_name2 via->xy)  
                                    via2 = axlCopyObject(via ?angle 0.0)
                                    new_via1 = axlReplacePadstack(via via_name1)
                                    new_via2 = axlReplacePadstack(via2 via_name2)
                                    axlDBAssignNet(new_via1 via_net t)
                                    axlDBAssignNet(new_via2 via_net t)
                                
                                )
                            ) 
                        )
                       
                    )                    
                        
                )
                close(Out_File)
                ;axlUIViewFileCreate("cut_padstack.log", "Show Element To Check Pins" nil)
            )
        )

)
;##################################################################### 
axlCmdRegister("chk2_0802" `chk2_0802)
(defun chk2_0802 ()
    Out_File = outfile("./chk2_0802.log" "w")
    fprintf(Out_File "---------------------\n" )
    foreach(net setof(e axlDBGetDesign()->nets e->diffpair == nil)
        foreach(branch net->branches
            find_pair_item =  setof(e branch->children (e->objType == "via" && index(e->name "VIA75B-") != nil))
            if(find_pair_item != nil then
                foreach(via find_pair_item
                    par_name_via = parseString(via->name "-")
                    new_name = strcat("VIA75-" cadr(par_name_via) "-" caddr(par_name_via))
                    l_pd1 = axlLoadPadstack(new_name)
                    if(l_pd1 != nil then 
                        fprintf(Out_File "change name successfully %-10s %-10s %L\n" via->name new_name via->xy)
                        axlReplacePadstack(via new_name)
                    else
                        fprintf(Out_File "NG %-10s %-10s %L\n" via->name new_name via->xy)
                    )
                    
                )
            )
        )
    )
    close(Out_File)
    axlUIViewFileCreate("chk2_0802.log", "Show Element To Check Pins" nil)
       
        
)
;##################################################################### 
axlCmdRegister("chk_0802" `chk_0802)
input_chk0802 = nil
(defun custom_value ()
    input_chk0802= axlEnterString(?prompts list("Please enter line Clearance:"))
)
(defun chk_0802 ()
    axlClearSelSet()
    axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
    mypopup = axlUIPopupDefine(nil (list  (list "Custom" 'custom_value) (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
    axlUIPopupSet( mypopup)
    view = nil
    while(axlSelect(?prompt "Please select an element...")
        STT =0
        if( view != nil then axlUIWClose(view) )
        Input_List = nil
        Input_List =  axlGetSelSet()
        Out_File = outfile("./chk_0802.log" "w")
        fprintf(Out_File "---------------------\n" )
        foreach(via setof(e Input_List (e->net->diffpair != nil && index(e->name "BOTTOM")== nil))
            if(input_chk0802 == nil then
                xy2 = axlMXYAdd(via->xy  0:315)
            else
                printf("%s\n" input_chk0802)
                case(input_chk0802
                    ("t"
                        xy2 = axlMXYAdd(via->xy  0:315)
                    )
                    ("b"
                        xy2 = axlMXYAdd(via->xy  315:0)
                    )
                    ("l"
                        xy2 = axlMXYSub(via->xy  315:0)
                    )
                    ("r"
                        xy2 = axlMXYSub(via->xy  0:315)
                    )
                
                )
            )
            
            ;xy2 = axlMXYSub(via->xy  315 :0)
            axlClearSelSet()
            axlSingleSelectPoint(xy2)
            if(setof(e axlGetSelSet()  e->net->name=="VSS")==nil then
                fprintf(Out_File "%L - %L - %L\n" xy2 via->xy axlGetSelSet())
            ; else
            ; foreach(via2 axlGetSelSet()
                 ; fprintf(Out_File "AA %L - %L - %L\n" xy2 via->xy via2->xy)
            ; )
            )  
        )
        close(Out_File)
        axlUIViewFileCreate("chk_0802.log", "Show Element To Check Pins" nil)
    )
       
        
)
;##################################################################### 
axlCmdRegister("rvbd" `replace_via_bydrc)
;axlSetFunckey( "6" "rvbd" t)
mypopup = nil
(defun  change_up () 
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Down" 'change_down)   ))
	axlShell("done")
	axlShellPost("rvbd")
)
(defun  change_down () 
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Up" 'change_up)  ))
	axlShell("done")
	axlShellPost("rvbd")
)
(defun replace_via_bydrc () 
    view = nil
    sub_info = car(setof(e axlVisibleGet() member("DRC ERROR CLASS" e) != nil ))->subclassinfo
    sub_vis = setof(e sub_info cadr(e) == t)
    printf("DRC length : %L\n" length( sub_vis))
    if(length( sub_vis)>1 then
      axlUIConfirm( "Visible layer DRCs was invalid\n Try again!" 'error )                
    else
        axlSetFindFilter(?enabled '(noall DRCS NAMEFORM) ?onButtons '(noall DRCS))
		if(mypopup != nil then
			if(setof(e mypopup car(e)=="Down") == nil then
			mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Up" 'change_up)  ))
			else
			mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Down" 'change_down)   ))
			)
		else
			mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Down" 'change_down)   ))
		)
        
        axlUIPopupSet( mypopup)
            while(  axlSelect(?prompt "Please select an element...") 
                sub_info = car(setof(e axlVisibleGet() member("DRC ERROR CLASS" e) != nil ))->subclassinfo
                sub_vis = setof(e sub_info cadr(e) == t)
                if(length( sub_vis)>1 then
                 axlUIConfirm( "Visible layer DRCs was invalid\n Try again!" 'error )     
                 else
                    Input_List = nil
                    STT=0
                    Input_List = setof(e axlGetSelSet()  e->name != "Differential Pair Max Uncoupled Length" )
                    Input_List = setof(e Input_List  e->name != "Maximum Neck Length" )
                    axlClearSelSet()
                    if(view != nil then axlUIWClose(view))
                        Out_File = outfile("./replace_via_by_drc.log" "w")
                        axlClearSelSet()
                        axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
                        model_data_list = nil
						exp_list = nil
						foreach(drc setof( e Input_List e->expected != nil)
							d_actual = atof(car(parseString(drc->expected " ")))
							if(d_actual != nil
								exp_list = cons(d_actual exp_list)
							)
							
						)
						exp_list = sort(exp_list 'lessp)
						println(lastelem(exp_list))
                        foreach(drc Input_List
							d_actual = 1.0
                            /*if(drc->actual=="OVERLAP" || atof(drc->actual) == nil ||  atof(drc->actual) <0.1 then
								if( atof(car(parseString(drc->expected " "))) != nil then
									d_actual = atof(car(parseString(drc->expected " ")))/2.0
								)
                                
                            else
								if( atof(car(parseString(drc->expected " "))) != nil then
									d_actual2 = atof(car(parseString(drc->expected " ")))/2.0
									if(d_actual<d_actual2 then
									d_actual = d_actual2
									d_actual = atof(drc->actual)
									)
								)
                                
                            )
							printf("%L\n" d_actual)*/
							via_list = nil
							if(drc->xy != nil then
								; if(drc->name == "Blind/Buried Via to Blind/Buried Via Spacing" then
									; d_actual = 25.0
								; else
									
									
								; )
								if(exp_list != nil then
										d_actual = lastelem(exp_list)		
									
								)
								if(d_actual ==0.0 then d_actual=1.0)
								d = axlMXYAdd( 0:d_actual drc->xy) 
								r_path = axlMakeDynamicsPath(list(list(d d 1.0 drc->xy d_actual t) list(d d) ) )
								poly1 = car(axlPolyFromDB(r_path ?layer drc->layer ?endCapType 'ROUND ?padType 'REGULAR))
								;axlDBCreateShape( poly1 t "ANTI ETCH/ALL" nil nil)
								axlClearSelSet()
								axlSingleSelectBox(poly1->bBox)
								sel_vias = setof(e axlGetSelSet() setof(e2 axlDBGetConnect(e t) e2->objType=="path" && index(e2->layer caar(sub_vis)) != nil) == nil) 
								if(length(sel_vias ) ==0 && index(drc->name "to Line Spacing") != nil then
									d_actual = (d_actual+25.0)*2.0
									d = axlMXYAdd( 0:d_actual drc->xy) 
									r_path = axlMakeDynamicsPath(list(list(d d 1.0 drc->xy d_actual t) list(d d) ) )
									poly1 = car(axlPolyFromDB(r_path ?layer drc->layer ?endCapType 'ROUND ?padType 'REGULAR))
									;axlDBCreateShape( poly1 t "ANTI ETCH/ALL" nil nil)
									axlClearSelSet()
									axlSingleSelectBox(poly1->bBox)
									via_list = setof(e axlGetSelSet() setof(e2 axlDBGetConnect(e t) e2->objType=="path" && index(e2->layer caar(sub_vis)) != nil) == nil) 
								else
									tmp = nil
									foreach(s_via  sel_vias
										tmp = cons(list(axlDistance(s_via->xy drc->xy) s_via) tmp)
									
									)
									tmp = sortcar(tmp 'lessp)
									if(length(tmp)==1 then
										via_list  =  list(cadr(car(tmp)) )
									else
										via_list  = list(cadr(car(tmp)) cadr(cadr(tmp)))
									)
									
								)
							)
							if(length(via_list )>0 then 
                            foreach(via setof(e via_list e->name != nil)
                                ;printf("%L\n" via->name)
                                par_string0 = nth(0 parseString(via->name "-") )
                                par_string1 = nth(1 parseString(via->name "-") )
                                par_string2 = nth(2 parseString(via->name "-") )
                                ;par_string3 = nth(3 parseString(via->name "-") )
								new_name_via = nil
                                if(index(via->name "BOTTOM" ) != nil || index(via->name "BOTOM" ) != nil then
									if(drc->layer != nil then
										par_string1 = cadr(parseString(drc->layer "/"))
									   
										if(index(par_string1 "VP") != nil then
											par_string1 = cadr(member(par_string1 axlSubclassRoute()))
										else
											par_string1 = cadr(member(par_string1 axlSubclassRoute(?field 'isEtch)))
										
										)
									   printf("%L\n" par_string1)
										printf("%L\n" par_string2)
										new_name_via = strcat(par_string0 "-" par_string1 "-" par_string2)
									
									)
                                    
                                    
                                else
									if(index(via->name "TOP" ) != nil then
										par_string2 = cadr(parseString(drc->layer "/"))
										if(index(par_string2 "VP") != nil then
											par_string2 = cadr(member(par_string2 reverse(axlSubclassRoute())))
										else
											par_string2 = cadr(member(par_string2 reverse(axlSubclassRoute(?field 'isEtch))))
										)
										if(par_string2 != nil 
											new_name_via = strcat(par_string0 "-" par_string1 "-" par_string2)
										)
                                    else
										
										subs = axlSubclassRoute(?field 'isEtch)
										if(drc->layer != nil then 
										par_string1_new = cadr(parseString(drc->layer "/"))
										
										if(index(par_string1_new "VP") != nil then
											par_string1_new = cadr(member(par_string1_new axlSubclassRoute()))
										)
										
										new_name_via2 = nil
										if(setof(e mypopup car(e)=="Down") == nil then 
											;new_name_via2 = strcat(par_string0 "1-" par_string1 "-" cadr(member(par_string1_new subs)))
											new_name_via2 = strcat(par_string0 "-" par_string1 "-" cadr(member(par_string1_new reverse(subs))) )
										else
											new_name_via2 = strcat(par_string0 "-" cadr(member(par_string1_new subs)) "-" par_string2)
										)
										par_tmp = parseString(via->name "-")
										tmp_name = new_name_via2
										if(length(par_tmp) ==4 && index(lastelem(par_tmp) "S") != nil then
											tmp_name = strcat(new_name_via2 "-" lastelem(par_tmp))
										)
										;println(tmp_name)
										pd_dbid = axlLoadPadstack(tmp_name)
										;println(tmp_name)
										if(pd_dbid != nil then
											if(axlDBIsFixed(via->net) != nil || axlDBIsFixed(via) != nil then
											else
											axlReplacePadstack(via tmp_name) 
											)
											
										)
										)
									)
                                    
                                )
								if(new_name_via != nil then 
									;new_name_via ="VIA50-TOP-MP50"
									pd_dbid = axlLoadPadstack(new_name_via)
									if(pd_dbid != nil then
										model_data_list = cons(list( via->name new_name_via via->xy via)  model_data_list)
									else
									   STT++
									   fprintf(Out_File "%-15s %-15s %L No found database in design\n"  via->name new_name_via via->xy ) 
									)
								)
                            ) 
							)
                        )
                        if(model_data_list != nil then
                            foreach(dbid unique(model_data_list)
                                if(axlDBIsFixed(lastelem(dbid)->net) == nil then
                                    if(axlDBIsFixed(lastelem(dbid)) == nil then 
                                        STT++
                                        fprintf(Out_File "%-15s %-15s %L\n"  car(dbid) cadr(dbid) caddr(dbid) ) 
										;axlReplacePadstack( lastelem(dbid) cadr(dbid)) 
										conn_pin_list = setof(e axlDBGetConnect(lastelem(dbid) t) e->objType=="pin")
										if(car(conn_pin_list) != nil then
											conn_via_list = setof(e axlDBGetConnect(car(conn_pin_list) t) e->objType=="via")
											foreach(conn_via setof(e conn_via_list axlDBIsFixed(e) == nil && axlDBIsFixed(e->net) == nil)
												axlReplacePadstack(conn_via cadr(dbid)) 
												;axlDBAddProp(conn_via list( "Comment" "mycoment"))
											)
										
										)
                                        
                                    )
                                )
                                
                            )
                        )
                    close(Out_File)
                    axlSetFindFilter(?enabled '(noall DRCS NAMEFORM) ?onButtons '(noall DRCS))
                    printf("Total: %n Pins Unconnect\n" STT)
                    ; if(STT>0 then
                        ; view= axlUIViewFileCreate("replace_via_by_drc.log", "Show Element To Check Pins dummy" nil)   
                        ; ;else
                            ; ;deleteFile("uni_via4.log")
                    ; )  
            )
        )
    )
)
;##################################################################### 

axlCmdRegister("replace_via_btn_by_drc" `replace_via_btn_by_drc)
;axlSetFunckey( "6" "replace_via_btn_by_drc" t)
(defun replace_via_btn_by_drc () 
    view = nil
    sub_info = car(setof(e axlVisibleGet() member("DRC ERROR CLASS" e) != nil ))->subclassinfo
    sub_vis = setof(e sub_info cadr(e) == t)
    printf("DRC length : %L\n" length( sub_vis))
    if(length( sub_vis)>1 then
      axlUIConfirm( "Visible layer DRCs was invalid\n Try again!" 'error )                
    else
        yesno = axlUIYesNo( "Holding on via bottom " "Replace via by_drc" )
        axlSetFindFilter(?enabled '(noall DRCS NAMEFORM) ?onButtons '(noall DRCS))
        mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
        axlUIPopupSet( mypopup)
            while(  axlSelect(?prompt "Please select an element...") 
                sub_info = car(setof(e axlVisibleGet() member("DRC ERROR CLASS" e) != nil ))->subclassinfo
                sub_vis = setof(e sub_info cadr(e) == t)
                if(length( sub_vis)>1 then
                 axlUIConfirm( "Visible layer DRCs was invalid\n Try again!" 'error )     
                 else
                    Input_List = nil
                    STT=0
                    Input_List = axlGetSelSet() 
                    axlClearSelSet()
                    if(view != nil then axlUIWClose(view))
                        Out_File = outfile("./replace_via_btn_by_drc.log" "w")
                        axlClearSelSet()
                        axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
                        model_data_list = nil
                        foreach(drc Input_List
                           ;printf("%L\n" drc->actual)
                            if(drc->actual=="OVERLAP" || atof(drc->actual) == nil then
                                d_actual = drc->expected
                            else
                                d_actual = atof(drc->actual)
                            )
                            d = axlMXYAdd( 0:d_actual drc->xy)
                            r_path = axlMakeDynamicsPath(list(list(d d 1.0 drc->xy d_actual t) list(d d) ) )
                            poly1 = car(axlPolyFromDB(r_path ?layer drc->layer ?endCapType 'ROUND ?padType 'REGULAR))
                            ;axlDBCreateShape( poly1 t "ANTI ETCH/ALL" nil nil)
                            axlClearSelSet()
                            axlSingleSelectBox(poly1->bBox)
                            Input_List2 = axlGetSelSet() 
                            ;printf("%L\n" Input_List2)
                            Input_List3 = nil 
                            ori_via = nil
                            if(yesno == t then
                                ori_via = car(setof(e Input_List2 index(e->name "BOTTOM" )!= nil))
                                Input_List3 = setof(e Input_List2 index(e->name "BOTTOM" )== nil)
                            else
                                ori_via = car(setof(e Input_List2 index(e->name "TOP" )!= nil))
                                Input_List3  = setof(e Input_List2 index(e->name "TOP" )== nil)
                            )  
                            if(ori_via != nil then
                                foreach(via Input_List3
                                    printf("%L\n" via->name)
                                    par_string0 = nth(0 parseString(via->name "-") )
                                    par_string1 = nth(1 parseString(via->name "-") )
                                    par_string2 = nth(2 parseString(via->name "-") )
                                    par_layer = cadr(parseString(drc->layer "/"))
                                    ;printf("%L\n" par_layer)
                                    chk_layer = axlLayerGet(strcat("ETCH/" par_layer))->isEtch 
                                    if(chk_layer== nil  then
                                        if(yesno == t then
                                             par_layer = cadr(member(par_layer reverse(axlSubclassRoute())))
                                        else
                                             par_layer = cadr(member(par_layer axlSubclassRoute()))
                                        )
                                    )
                                    ;printf("%L\n" par_layer)
                                    if(yesno == t then
                                        if(via->net->name==ori_via->net->name then
                                            new_name_via = strcat(par_string0 "-" par_string1 "-" par_layer)
                                        else
                                            par_layer = cadr(member(par_layer reverse(axlSubclassRoute(?field 'isEtch))))
                                            new_name_via = strcat(par_string0 "-" par_layer1 "-" par_layer)
                                        )
                                    else
                                       
                                        if(via->net->name==ori_via->net->name then
                                            new_name_via = strcat(par_string0 "-" par_layer "-" par_string2)
                                        else
                                            par_layer = cadr(member(par_layer axlSubclassRoute(?field 'isEtch)))
                                            new_name_via = strcat(par_string0 "-" par_layer "-" par_string)
                                        )
                                        
                                    )
                                    
                                    pd_dbid = axlLoadPadstack(new_name_via)
                                    ;printf("%L\n" via->name)
                                    if(pd_dbid != nil then
                                        model_data_list = cons(list( via->name new_name_via via->xy via)  model_data_list)
                                    else
                                        printf("%L\n" new_name_via)
                                    ) 
                                )
                            )
                            
                        )
                        if(model_data_list != nil then
                            foreach(dbid unique(model_data_list)
                                STT++
                                fprintf(Out_File "%-15s %-15s %L\n"  car(dbid) cadr(dbid) caddr(dbid) ) 
                                axlReplacePadstack(lastelem(dbid) cadr(dbid)) 
                            )
                        )
                    close(Out_File)
                    axlSetFindFilter(?enabled '(noall DRCS NAMEFORM) ?onButtons '(noall DRCS))
                    printf("Total: %n Pins Unconnect\n" STT)
                    if(STT>0 then
                        view= axlUIViewFileCreate("replace_via_btn_by_drc.log", "Show Element To Check Pins dummy" nil)   
                        ;else
                            ;deleteFile("uni_via4.log")
                    )  
            )
        )
    )
)
;##################################################################### 
axlCmdRegister("hl_bbvia" `hl_bbvia)
(defun hl_bbvia ()
    axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
    mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
    axlUIPopupSet( mypopup)
        while(axlSelect(?prompt "Please select an element...") 
            Input_List = nil
            Input_List = setof(e axlGetSelSet() index(  e->name "-" ) != nil)
            axlClearSelSet()
               foreach(via Input_List
                   st_via = cadr(parseString(car(via->startEnd) "/") )
                   end_via = cadr(parseString(cadr(via->startEnd) "/") )
                   layer_list = member(st_via axlSubclassRoute(?field 'isEtch))
                   layer_list = reverse(member(end_via reverse(layer_list)))
                   if(length(layer_list)==2 then
                   axlCustomColorObject(via) 
                   )
                           
               ) 
               
        ) 

    )  
;#####################################################################  
axlCmdRegister("rsv2" `rsv2)
    input_rsv2 = "VIA50 IN"
(   defun rsv2 ()
	sub_info = car(setof(e axlVisibleGet() member("DRC ERROR CLASS" e) != nil ))->subclassinfo
    sub_vis = setof(e sub_info cadr(e) == t)
    printf("DRC length : %L\n" length( sub_vis))
		if(length( sub_vis)>1 then
		axlUIConfirm( "Visible layer DRCs was invalid\n Try again!" 'error )                
    else
		if( input_rsv2 != nil then
				input_string = axlUIPrompt("Enter parameters via size - In/Out:" input_rsv2 )
			else
				input_string = axlUIPrompt("Enter parameters via size - In/Out:" "VIA50 IN")
		)
		if(input_string != nil then
			input_string = parseString(upperCase(input_string) " ")
			axlClearSelSet()
			axlSetFindFilter(?enabled '(noall LINES) ?onButtons '(noall LINES))
			mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
			axlUIPopupSet( mypopup)
			while(axlSelect(?prompt "Please select an element...")
				STT =0
				if(view != nil then axlUIWClose(view))
				Input_List = nil
				Input_List =  axlGetSelSet()
				Out_File = outfile("./rsv2.log" "w")
				poly_list = nil
				sub_info = car(setof(e axlVisibleGet() member("DRC ERROR CLASS" e) != nil ))->subclassinfo
				sub_vis = setof(e sub_info cadr(e) == t)
			
				foreach(line Input_List
					p = axlPolyFromDB(line ?endCapType 'ROUND  ?line2poly t)
					poly_list = cons(car(p) poly_list)
				)
				poly_hole_list = nil
				or_poly = axlPolyOperation(car(poly_list) cdr(poly_list) 'OR)
				 ;-------------------------------------------------------------
				poly_all = nil
				foreach(poly or_poly
					hole = poly->holes
					if(hole == nil then
						poly_all = cons(poly poly_all)
					else
						poly2 = nil
						foreach(h hole 
							if(axlPolyFromHole(h) != nil then
								poly2 = cons(car(axlPolyFromHole(h)) poly2)
							)  
						)
						if(poly2 != nil then 
							 opp = axlPolyOperation(poly poly2 'OR)
							 if(length(opp)>1 then 
								poly_all = cons(car(opp) poly_all) 
								poly_all = cons(cadr(opp) poly_all) 
							 
							 else
								poly_all = cons(car(opp) poly_all) 
							 )    
						) 
					)            
				)
				if(poly_all != nil then
					p_total = axlPolyOperation( car(poly_all) cdr(poly_all) 'OR)
					ori_vis = axlVisibleGet()
					axlVisibleDesign(nil)
					if(sub_vis == nil then
					axlVisibleLayer( "VIA CLASS"  t)
					else
					axlVisibleLayer( strcat("VIA CLASS/" caar(sub_vis))  t)
					)
					
					axlClearSelSet()
					axlSetFindFilter(?enabled '(noall VIAS) ?onButtons '(noall VIAS))
					axlAddSelectAll()
					;via_design_list = setof(e axlGetSelSet() axlDBIsFixed(e->net) == nil && e->prop->COMMENT =="guard_via" || e->prop->COMMENT =="TIP_VIA" || e->prop->COMMENT =="TIP-VIA")
					via_design_list = setof(e axlGetSelSet() index(e->name "TOP") == nil &&  axlDBIsFixed(e->net) == nil && axlDBIsFixed(e) == nil )
					axlVisibleSet(ori_vis)
					println(length(via_design_list))
					foreach(via via_design_list
						if(cadr(input_string)=="IN" then
							if(cadr(input_string)=="IN" then
									tmp = setof(e p_total axlGeoPointInShape(via->xy e nil) == t )
								if(length(tmp) >0 && index(via->name car(input_string)) == nil then
									STT++
									fprintf(Out_File "%-5d %L %-10s %-15L %-L\n" STT via->prop->COMMENT via->name sprintf(nil "%s%s" car(input_string) index(via->name "-")) via->xy )
									;axlReplacePadstack(via sprintf(nil "%s%s" car(input_string) index(via->name "-")) )
								)
							 ) 
							
						else
							if(cadr(input_string)=="OUT" then
									tmp = setof(e p_total axlGeoPointInShape(via->xy e nil) == nil )
								if(length(tmp) == length(p_total) && index(via->name car(input_string)) == nil then
									STT++
									fprintf(Out_File "%-5d %L %-10s %-15L %L\n" STT via->prop->COMMENT via->name sprintf(nil "%s%s" car(input_string) index(via->name "-")) via->xy )
									;axlReplacePadstack(via sprintf(nil "%s%s" car(input_string) index(via->name "-")) )
									
									
								
								)
							 ) 
							
						)
						
					)
					; foreach(poly p_total
						; ;axlDBCreateShape(poly t "ANTI ETCH/TOP" nil nil)
						; if(cadr(input_string)=="IN" then
							; via_list = setof(e via_design_list (axlGeoPointInShape(e->xy poly nil)== t && index(e->name car(input_string)) == nil ))
						; else
							; if(cadr(input_string)=="OUT" then
								; via_list = setof(e via_design_list (axlGeoPointInShape(e->xy poly nil)== nil && index(e->name car(input_string)) == nil ))
							; ) 
						; )
						
						; if(via_list != nil then
							; foreach(via setof(e via_list  axlDBIsFixed(e) == nil)
								; par_string = parseString(via->name "-")
								; ps_name = car(input_string)
								; foreach(s cdr(par_string)
									; ps_name =  strcat(ps_name "-" s)
								; )
								; STT++
								; fprintf(Out_File "%-5d %-10s %-10s %-15L\n" STT via->name ps_name via->xy )
								; axlReplacePadstack(via ps_name )
							; )
						
						; ) 
					 
					 ; )
					  
				)
				;-------------------------------------------------------------
				close(Out_File)
				printf("Total: %n Found\n" STT)
				if( STT >0 then
				view= axlUIViewFileCreate("rsv2.log", " " nil)
				else
				axlUIConfirm( "No Found!")
				)
				
			)
		else
			printf("Try again\n")
		)
    )
)
;#####################################################################
axlCmdRegister("seri_conn_net", 'seri_conn_net)
(defun seri_conn_net ()
    view = nil
    axlClearSelSet()
    axlSetFindFilter(?enabled '(noall SYMBOLS VIAS NAMEFORM) ?onButtons '(noall VIAS))
    mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
    axlUIPopupSet( mypopup)
        while(axlSelect(?prompt "Please select an element...")
            Input_List = nil
            Input_List = axlGetSelSet() 
            STT=0
            if(view != nil then axlUIWClose(view))
                Out_File = outfile("./seri_conn_net.log" "w")
               foreach(via Input_List
                    lpins = axlPinsOfNet(via->net 'pins)
                    st_char = nil
                    foreach(pin lpins st_char = cons(substring(pin->parent->refdes 1 1) st_char) )
                    if(length(unique(st_char))> 1 then
                        via_connected = setof(e axlDBGetConnect(via t) e->objType=="path")
                        via_pin_connected = setof(e axlDBGetConnect(via t) e->objType=="pin")
                        refdes_pin = car(via_pin_connected)->parent->refdes
                        if(via_connected != nil then
                            foreach(cline via_connected
                                cline_connected = setof(e axlDBGetConnect(cline t) e->objType=="via" && e != via)
                                pin_conn = nil
                                if(cline_connected != nil then
                                    via2_connected = setof(e axlDBGetConnect(car(cline_connected) t) e->objType=="pin") 
                                    if(via2_connected != nil then
                                        pin_conn = car(via2_connected)
                                    else
                                        via3_connected = setof(e axlDBGetConnect(car(cline_connected) t) e->objType=="shape")
                                        via22_connected = setof(e axlDBGetConnect(car(via3_connected) t) e->objType=="pin") 
                                        pin_conn = car(via22_connected)
                                    )
                                    chk1 = substring(pin_conn->parent->refdes 1 1)
                                    if(chk1=="U" then
                                    STT++
                                    fprintf(Out_File, "%-5n %-15s %-15L %-15L\n" STT pin_conn->parent->refdes st_char   via->xy)
                                    ) 
                                )
                            )   
                        )
                    )       
               ) 
                close(Out_File)
                printf("Total: %n Pins Unconnect\n" STT)
                 if(STT>0 then
                    view= axlUIViewFileCreate("seri_conn_net.log", "Show Element To Check Pins dummy" nil) 
                )
        ) 
)
;########################################################################
axlCmdRegister("replace_via_by_drc" `replace_via_by_drc)
(defun replace_via_by_drc () 
    view = nil
    input= nil
    sub_info = car(setof(e axlVisibleGet() member("DRC ERROR CLASS" e) != nil ))->subclassinfo
    sub_vis = setof(e sub_info cadr(e) == t)
    if(length( sub_vis)>1 then
      axlUIConfirm( "Visible layer DRCs was invalid\n Try again!" 'error )                
    else
        input= axlEnterString(?prompts list("Please enter params :"))
        pad_list = nil
            foreach( padstack axlDBGetDesign()->padstacks
                pad_list = cons(list(padstack->startEnd padstack) pad_list)
            )
        subs = axlSubclassRoute(?field 'isEtch)
    )
    if(input != nil then
        input = parseString(upperCase(input))
        axlSetFindFilter(?enabled '(noall DRCS NAMEFORM) ?onButtons '(noall DRCS))
        mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
        axlUIPopupSet( mypopup)
            while(axlSelect(?prompt "Please select an element...") 
                Input_List = nil
                STT=0
                Input_List = axlGetSelSet() 
                axlClearSelSet()
                if(view != nil then axlUIWClose(view))
                    Out_File = outfile("./replace_via_by_drc.log" "w")
                    axlClearSelSet()
                    axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
                    sel_points = nil
                    sel_points2 = nil
                    foreach(drc Input_List
                        axlClearSelSet()
                        axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
                        axlAddSelectPoint(drc->xy)
                        expect = atof(car(parseString(drc->expected " ")))
                        foreach(via axlGetSelSet()
                         sel_points = cons(list(via expect) sel_points)
                         actual = nil
                          if(drc->actual =="OVERLAP" then
                            actual = 0.0
                          else
                            actual = atof(car(parseString(drc->actual " ")))
                          )
                         sel_points2 = cons(list(via expect actual) sel_points2)
                        )
                      )
                      foreach(via sel_points2
                        if(cadr(via) != nil then
                            poly_via = car(axlPolyFromDB(car(via) ?layer strcat("ETCH/" caar(sub_vis) ) ?endCapType 'ROUND ?padType 'REGULAR))
                            size_via = (xCoord(cadr(poly_via->bBox)) - xCoord(car(poly_via->bBox)))/2
                            Item_poly_Expand = car(axlPolyExpand(poly_via (cadr(via)+size_via) 'NONE))
                            ;axlDBCreateShape(Item_poly_Expand t strcat("ANTI ETCH/" caar(sub_vis) )  nil)
                            axlClearSelSet()
                            axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
                            axlAddSelectBox(Item_poly_Expand->bBox)
                            foreach( e axlGetSelSet()
                                air = axlAirGap(e car(via)  strcat("ETCH/" caar(sub_vis)))
                                if(air != nil then
                                    if(abs(lastelem(via) - lastelem(air)) <0.001 then
                                    sel_points = cons(list(e nil) sel_points)
                                    )
                                )
                                
                            )
                        )
                      )
                    foreach(via  setof( e sel_points  (member(strcat("ETCH/" car(input)) car(e)->startEnd) == nil && axlDBIsFixed(car(e))== nil ) )
                        name_temp = nil
                        if(index(car(via)->name "TOP") != nil then
                            name_temp = list("ETCH/TOP" strcat("ETCH/" car(input)) )
                        else
                            if(index(car(via)->name "BOTTOM") != nil then 
                                name_temp = list( strcat("ETCH/" car(input)) "ETCH/BOTTOM" )
                            )
                        )
                        if(name_temp != nil then
                            sub_1 = cadr(parseString(car(name_temp) "/"))
                            sub_2 = cadr(parseString(cadr(name_temp) "/"))
                            trim1 = member(cadr(parseString(car(car(via)->startEnd) "/"))  subs)
                            trim2 = member(cadr(parseString(cadr(car(via)->startEnd) "/"))  reverse(trim1))
                            pad_layer1 = car(name_temp)
                            pad_layer2 = cadr(name_temp)
                            if(member(sub_1 trim2) == nil then
                                pad_layer1 = cadr(parseString(car(car(via)->startEnd) "/"))
                            )
                            if(member(sub_2 trim2) == nil then
                                pad_layer2 = cadr(parseString(cadr(car(via)->startEnd) "/"))
                            )
                            chk1 = axlDBGetPad(car(via) pad_layer1 "ANTI")
                            chk2=  axlDBGetPad(car(via) pad_layer2 "ANTI")
                            find_pad = setof(e pad_list (car(e)== name_temp))
                            foreach(pad find_pad
                                   chk_1 = axlDBGetPad(cadr(pad) caar(pad) "ANTI")
                                   chk_2 = axlDBGetPad(cadr(pad) cadr(car(pad)) "ANTI")
                                   if(list(chk_1->bBox chk_2->bBox) == list(chk1->bBox chk2->bBox) then
                                     STT++  
                                     fprintf(Out_File "%-15s %-15s %-15L\n" car(via)->name cadr(pad)->name  car(via)->xy)
                                     axlReplacePadstack(car(via) cadr(pad)->name)
                                   )  
                            )
                        )
                    )
                    close(Out_File)
                    axlSetFindFilter(?enabled '(noall DRCS NAMEFORM) ?onButtons '(noall DRCS))
                    printf("Total: %n Pins Unconnect\n" STT)
                     if(STT>0 then
                        view= axlUIViewFileCreate("replace_via_by_drc.log", "Show Element To Check Pins dummy" nil)   
                        ;else
                            ;deleteFile("uni_via4.log")
                    )
            )
    ) 
)
;########################################################################
axlCmdRegister("uni_via6" `uni_via6)
input_uni_via6 = nil
(defun uni_via6 ()
	if(input_uni_via6 == nil then
		temp = axlUIPrompt("Enter parameters :" input_uni_via6 )
		if(temp != nil then input_uni_via6 = temp )
	else
		temp = axlUIPrompt("Enter parameters :" input_uni_via6 )
		if(temp != nil then input_uni_via6 = temp )
	)
	input_file = axlDMFileBrowse(nil nil ?title "Please choose file create Pins ..." ?optFilters "All text files|*.txt|")
    Config_via = nil
    air_gap_max = 0
    if(input_file != nil then
        inPort = infile(input_file)	
         when(inPort
                while(gets(nextline inPort)
                    if(length(parseString(nextline)) >=1 then
                        parse_row = parseString(nextline, "\n")
                        parse_row2 = parseString(car(parse_row), " ")
                        if(atof(caddr(parse_row2))> air_gap_max then
                            air_gap_max = atof(caddr(parse_row2))
                        )
                        Config_via = cons(list(list(atof(car(parse_row2)) atof(cadr(parse_row2))) atof(caddr(parse_row2) )) Config_via)
                        
                    else
                        printf("Stoped! Invalid database.\n")
                    );end if 
                                    
                );end while
            );end when
	)
	if(Config_via != nil then
		axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
		mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
		axlUIPopupSet( mypopup)
		dem=0
		Input_List = nil
		while(axlSelect(?prompt "Please select an element...")
		if( Input_List != nil then axlUIWClose(view))
		dem=0
		Input_List = nil
		Input_List = setof(e axlGetSelSet()  axlPadOnLayer(e strcat("ETCH/" input_uni_via6  ) t) == t)
		axlClearSelSet()
		Out_File = outfile("./uni_via6.log" "w")
		fprintf(Out_File "%L %L\n" Input_List input_uni_via6)
			if(Input_List != nil then
				via = car(Input_List)
				d = axlMXYAdd( 0:air_gap_max via->xy) 
				r_path = axlMakeDynamicsPath(list(list(d d 1.0 via->xy air_gap_max t) list(d d) ) )
				poly1 = car(axlPolyFromDB(r_path ?layer strcat("ANTI ETCH/" input_uni_via6  ) ?endCapType 'ROUND))
				;axlDBCreateShape( poly1 t strcat("ANTI ETCH/" input_uni_via6  ) nil nil)
				axlClearSelSet()
                axlSetFindFilter(?enabled '(noall VIAS) ?onButtons '(noall VIAS))
				axlSingleSelectBox(poly1->bBox)
				get_via_list = setof(e axlGetSelSet() e->xy != via->xy && axlGeoPointInShape(e->xy poly1 nil) == t )
				if(get_via_list != nil then
					dis_list = nil
					contact_size_via_list = nil
					f_name_via = car(parseString(via->name "-"))
					foreach(via2 get_via_list
						dis = axlDistance(via2->xy via->xy)
						f_name_via2 = car(parseString(via2->name "-"))
						temp_list = list(split_number(f_name_via)  split_number(f_name_via2))
						check_size = setof(e Config_via car(e) == temp_list || car(e)==reverse(temp_list))
						if(check_size != nil then
							;if(dis<cadr(car(check_size)) then
								fprintf(Out_File "%L %L - %L\n" dis temp_list Config_via )
							;)	
						)
						;fprintf(Out_File "%L %L\n" list(f_name_via f_name_via2) Config_via)
						;dis_list = cons(list()))
					)
				)
				
				
				
				
			)
			fprintf(Out_File, "%s\n", "-------------------------------------------------")
			fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
			fprintf(Out_File, "%s\n", "-------------------------------------------------")
			dem=0
			close(Out_File)
			printf("Total: %n Pins Unconnect\n" dem)
			view= axlUIViewFileCreate("uni_via6.log", "Show Element To Check Pins dummy" nil)
		)
		
	)
)
(defun split_number (str)
	get_number = nil
	foreach(arg parseString(str "")
		if(rexMatchp("^[0-9]*[.]*[0-9]+$" arg) then
			get_number = cons(arg get_number)
		
		)	
	)
	convert_str_num = nil
	convert_str_num = atof(buildString(reverse(get_number) ""))
	
)
;########################################################################
axlCmdRegister("uni_via5" `uni_via5)
input_uni_via5 = nil
input_cus_uni_via5 = 1
(defun custom_value1 ()
    input_cus_uni_via5= 1
)
(defun custom_value2 ()
    input_cus_uni_via5= 2
)
(defun uni_via5 ()
    view = nil
    ;input_uni_via5 = parseString(axlEnterString(?prompts list("Please enter params :")) " ")
    if( input_uni_via5 != nil then
        input_uni_via5 = axlUIPrompt("Enter parameters :" input_uni_via5 )
    else
        input_uni_via5 = axlUIPrompt("Enter parameters:")
    )
    input = parseString(input_uni_via5 " ")
    axlClearSelSet()
    axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
    mypopup = axlUIPopupDefine(nil (list (list "Case1" 'custom_value1) (list "Case2" 'custom_value1) (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
    axlUIPopupSet( mypopup)
        while(axlSelect(?prompt "Please select an element...") 
            
            Input_List = nil
            STT=0
            Input_List = setof(e axlGetSelSet() index(  e->name "-" ) != nil )
            axlClearSelSet()
            if(view != nil then axlUIWClose(view))
            Out_File = outfile("./uni_via5.log" "w")
            foreach(via Input_List
                pad_box = axlPolyFromDB(via ?endCapType 'ROUND ?layer strcat("ETCH/" car(input)) ?padType 'REGULAR)
                if( pad_box != nil then
                    axlClearSelSet()
                    axlSetFindFilter(?enabled '(noall VIAS) ?onButtons '(noall VIAS))
                    Item_poly_Expand = car(axlPolyExpand(car(pad_box) atof(cadr(input)) 'NONE)) 
                    ;axlDBCreateShape(Item_poly_Expand t strcat("ANTI ETCH/" car(input)) nil)
                    box_via = Item_poly_Expand->bBox
                    axlSingleSelectBox(box_via)
                    Input_List1 = setof(e axlGetSelSet() e != via)
                    if(length( Input_List1 ) >0 then
                        ;ori_via = car(Input_List1)
                        dis_via_list = nil
                        foreach(via1 Input_List1
                            dis_via_list = cons(list(axlDistance(via->xy via1->xy ) via1) dis_via_list)
                        )
                        dis_via_list = car(sortcar(dis_via_list 'lessp))
                        ori_via = cadr(dis_via_list)
                        st_via1 = cadr(parseString(car(via->startEnd) "/"))
                        st_via2 = cadr(parseString(car(ori_via->startEnd) "/"))
                        temp_value1 = length(member(st_via1 axlSubclassRoute(?field 'isEtch)))
                        temp_value2 = length(member(st_via2 axlSubclassRoute(?field 'isEtch)))
						if( axlDBIsFixed(via) == nil then
						 if(input_cus_uni_via5 ==1 then
                            if(temp_value1 < temp_value2 then
                            axlTransformObject(via ?move axlMXYSub(ori_via->xy via->xy))
                            ) 
                        else
                            if(temp_value1 > temp_value2 then
                            axlTransformObject(via ?move axlMXYSub(ori_via->xy via->xy))
                            ) 
                        
                        )
						
						)
                       
                         
                    )
                )
                 
            ) 
            close(Out_File)
            printf("Total: %n Pins Unconnect\n" STT)
            if(STT>0 then
               view= axlUIViewFileCreate("uni_via5.log", "Show Element To Check Pins dummy" nil)   
                
            )
        ) 

) 
;########################################################################
axlCmdRegister("uni_via4" `uni_via4)
(defun uni_via4 ()
    view = nil
    input= nil
    input= parseString(axlEnterString(?prompts list("Please enter params :")) " ")
    axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall PINS))
    mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
    axlUIPopupSet( mypopup)
        while(axlSelect(?prompt "Please select an element...") 
            Input_List = nil
            STT=0
            Input_List = axlGetSelSet()
            axlClearSelSet()
            if(view != nil then axlUIWClose(view))
                Out_File = outfile("./uni_via4.log" "w")
                foreach(pin setof(e Input_List ( axlDBIsFixed(e) != nil && length(axlDBGetConnect(e t))>1 && member("ETCH/BOTTOM" e->startEnd) != nil) )
                    poly = car(axlPolyFromDB(pin ?endCapType 'ROUND ?layer "ETCH/BOTTOM" ?padType 'REGULAR))
                    get_mask_pin = setof(e pin->pads (nindex( e->layer "SOLDERMASK" ) != nil && e->figure != nil))
                    if(get_mask_pin != nil then
                        solder_mask_pin = car(get_mask_pin) 
                        poly2 = car(axlPolyFromDB(car(solder_mask_pin->figure) ?layer car(pin->startEnd) ?endCapType 'ROUND ?padType 'REGULAR))
                    else
                        poly2 = poly
                    )
                    via_top = nil
                    Via_bottom = nil
                    axlClearSelSet()
                    axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
                    axlAddSelectBox(poly2->bBox)
                    axlShell("Redraw")
                    via_top= setof(e axlGetSelSet()  (index(e->name "TOP") != nil) )
                    via_bottom = setof(e axlGetSelSet() (index(e->name "BOTTOM") != nil) )
                    xy_selected = nil
                    if( length(via_bottom)>1 then
                        foreach(via_btm  via_bottom
                            dis_min = nil
                            foreach(via_t via_top
                                dis = axlDistance(via_btm->xy via_t->xy)
                                dis_min = cons( list(dis via_t->xy) dis_min) 
                            )
                            exsit_chk = setof(e dis_min (member(cadr(e) xy_selected) != nil) )
                            _data = sortcar( setof(e dis_min (member(cadr(e) xy_selected) == nil) ) 'lessp )
                            if(_data != nil then
                                data = car(_data)
                                hasChange = nil
                                if(car(data) < atof(car(input)) && car(data) !=0.0 then
                                    hasChange = t
                                else
                                    if(exsit_chk != nil then
                                        hasChange = t
                                    )
                                )
                                if(hasChange == t then
                                    xy_selected = cons(cadr(data) xy_selected)
                                    xy_temp = via_btm->xy
                                    via_tranf = axlTransformObject(via_btm ?move axlMXYSub(cadr(data) via_btm->xy))
                                    poly_ = car(axlPolyFromDB(via_tranf ?endCapType 'ROUND ?layer "ETCH/BOTTOM" ?padType 'REGULAR))
                                    join_poly = axlPolyOperation(poly2 poly_ 'OR)
                                    if(car(join_poly)->area > poly2->area || length(join_poly) > 1 then
                                        axlTransformObject(via_tranf ?move axlMXYSub(xy_temp via_tranf->xy ))
                                    else
                                        STT++
                                        fprintf(Out_File "%-8.4f %-15L %-15L\n" car(data) via_btm->xy cadr(data) )
                                         axlHighlightObject(via_tranf)
                                    ) 
                                )
                            )  
                        )
                    )  
                )
                close(Out_File)
                axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall PINS))
                printf("Total: %n Pins Unconnect\n" STT)
                 if(STT>0 then
                    view= axlUIViewFileCreate("uni_via4.log", "Show Element To Check Pins dummy" nil)   
                    ;else
                        ;deleteFile("uni_via4.log")
                )
        ) 

    ) 
    
;########################################################################
axlCmdRegister("uni_via3" `uni_via3)
(defun uni_via3 ()
    view = nil
    axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
    mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
    axlUIPopupSet( mypopup)
        while(axlSelect(?prompt "Please select an element...") 
            Input_List = nil
            STT=0
            ori_via = nil
            Input_List = setof(e axlGetSelSet() index(  e->name "-" ) != nil)
            axlClearSelSet()
            if(view != nil then axlUIWClose(view))
                Out_File = outfile("./uni_via3.log" "w")
                ori_via = nil
                via_top = setof(e Input_List (index(e->name "TOP") != nil) )
                if(via_top != nil then
                    ori_via = car(via_top)
                else
                    via_bottom = setof(e Input_List (index(e->name "BOTTOM") != nil) )
                    if(via_bottom != nil then
                        ori_via = car(via_bottom)
                    else
                    
                        via_fixed = setof(e Input_List (axlDBIsFixed(e) != nil) )
                        if(via_fixed != nil then
                             ori_via = car(via_fixed)
                        else
                            temp_value = 0
                            foreach(via Input_List
                                st_via = cadr(parseString(car(via->startEnd) "/"))
                                temp_value1 = length(member(st_via axlSubclassRoute(?field 'isEtch)))
                                if(temp_value1 > temp_value then
                                    ori_via = via
                                    temp_value = temp_value1
                                )
                            
                            )
                            
                        ) 
                    )
                )
                println(ori_via->name)
               foreach(item setof(e Input_List (e != ori_via && axlDBIsFixed(e) == nil ))
                    STT++
                    fprintf(Out_File, "%-5n%-15s %-15L%-15L\n" STT item->name item->xy ori_via->xy)
                    axlTransformObject(item ?move axlMXYSub(ori_via->xy item->xy)) 
                    ori_via = nil
                          
               ) 
                close(Out_File)
                printf("Total: %n Pins Unconnect\n" STT)
                ; if(STT>0 then
                   ; view= axlUIViewFileCreate("uni_via3.log", "Show Element To Check Pins dummy" nil)   
                    ;else
                        ;deleteFile("uni_via3.log")
                ;)
        ) 

    ) 
;########################################################################
axlCmdRegister("uni_via2" `uni_via2)
(defun uni_via2 ()
    view = nil
    pre_pads = nil
    foreach(padstack axlDBGetDesign()->padstacks
       if(index(  padstack->name "-" ) != nil then
           pre_pads = cons(car(parseString(padstack->name "-"))  pre_pads)
       )
        
    )
    pre_pads = unique(pre_pads)
    input_file = axlDMFileBrowse(nil nil ?title "Please choose file create Pins ..." ?optFilters "All text files|*.txt|")
    Config_via = nil
    air_gap_max = 0
    if(input_file != nil then
        inPort = infile(input_file)	
         when(inPort
                while(gets(nextline inPort)
                    if(length(parseString(nextline)) >=1 then
                        parse_row = parseString(nextline, "\n")
                        parse_row2 = parseString(car(parse_row), " ")
                        if(atof(caddr(parse_row2))> air_gap_max then
                            air_gap_max = atof(caddr(parse_row2))
                        )
                        Config_via = cons(list(list(strcat(concat("VIA" atoi(car(parse_row2)) )) strcat(concat("VIA" atoi(cadr(parse_row2))) )) caddr(parse_row2) ) Config_via)
                    else
                        printf("Stoped! Invalid database.\n")
                    );end if 
                                    
                );end while
            );end when 
        if(Config_via != nil then
            axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
            mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
            axlUIPopupSet( mypopup)
                while(axlSelect(?prompt "Please select an element...") 
                Input_List = nil
                STT=0
                Input_List = setof(e axlGetSelSet() index(  e->name "-" ) != nil)
                axlClearSelSet()
                if(view != nil then axlUIWClose(view))
                Out_File = outfile("./uni_via2.log" "w")
                checked_list = nil 
                delete_list = nil
                    foreach(item Input_List  
                        radius_sel = air_gap_max
                        left_side = axlMXYSub(item->xy list(radius_sel -radius_sel))
                        right_side = axlMXYAdd(item->xy list(radius_sel -radius_sel))
                        axlSingleSelectBox(list(left_side right_side))
                        foreach(via setof(e axlGetSelSet() e != item && member(e checked_list)== nil && axlDistance(e->xy item->xy)<air_gap_max)
                            dis_via = axlDistance(via->xy item->xy)
                            via1 = car(parseString(item->name "-"))
                            via2 = car(parseString(via->name "-"))
                            temps = setof(e Config_via (list(via1 via2) == car(e)))
                            if(temps != nil then
                                re_airgap = atof(lastelem(car(temps)))
                                if(dis_via < re_airgap then
                                    if(dis_via > 0 then
                                        if(axlDBIsFixed(item) == nil && setof(e axlDBGetProperties(item) lindex('(VIAARRAYID) car(e))!= nil  )!= nil then
                                            delete_list = cons(list(dis_via re_airgap item) delete_list)
                                        )  
                                    )
                                    checked_list = cons(item checked_list)

                                   
                                )
                                
                            )  
                            
                        ) 
                    ) 
                    foreach(item unique(delete_list)
                        STT++
                        fprintf(Out_File, "%n %-15.4f %-15.4f %-15L\n" STT car(item) cadr(item) caddr(item)->xy)
                         axlDeleteObject(caddr(item))
                    
                    )
                    close(Out_File)
                    printf("Total: %n Pins Unconnect\n" STT)
                    if(STT>0 then
                        view= axlUIViewFileCreate("uni_via2.log", "Show Element To Check Pins dummy" nil)   
                    ;else
                        ;deleteFile("uni_via2.log")
                    )
                ) 
        )
        else
             print("Not found") 
        ) 

    ) 
;########################################################################
axlCmdRegister("uni_via" `uni_via)
(defun uni_via ()
    view = nil
    pre_pads = nil
    ;input= nil
    ;input= parseString(axlUIPrompt("Enter parameters via size - In/Out:" "TOP" ))
	axlSetFindFilter(?enabled '(noall LINES ) ?onButtons '(noall LINES))
	axlSingleSelectName("PROPERTY" list( list("COMMENT") "uni_via" ))
	axlDeleteObject(axlGetSelSet())
	axlClearSelSet()
    foreach(padstack axlDBGetDesign()->padstacks
       if(index(  padstack->name "-" ) != nil then
           pre_pads = cons(car(parseString(padstack->name "-"))  pre_pads)
       )
        
    )
    pre_pads = unique(pre_pads)
    ;input_file = axlDMFileBrowse(nil nil ?title "Please choose file create Pins ..." ?optFilters "All text files|*.txt|")
	Config_via = nil
    air_gap_max = 0
	inPort = infile("./Config_via.txt")	
    if(inPort != nil then
        inPort = infile("./Config_via.txt")	
         when(inPort
                while(gets(nextline inPort)
                    if(length(parseString(nextline)) >=1 then
                        parse_row = parseString(nextline, "\n")
                        parse_row2 = parseString(car(parse_row), " ")
                        if(atof(caddr(parse_row2))> air_gap_max then
                            air_gap_max = atof(caddr(parse_row2))
                        )
                        Config_via = cons(list(list(atof(car(parse_row2)) atof(cadr(parse_row2))) atof(caddr(parse_row2) )) Config_via)
                        
                    else
                        printf("Stoped! Invalid database.\n")
                    );end if 
                                    
                );end while
            );end when 
        if(Config_via != nil then
            axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
            mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
            axlUIPopupSet( mypopup)
                while(axlSelect(?prompt "Please select an element...") 
                Input_List = nil
                STT=0
                Input_List = setof(e axlGetSelSet() index(  e->name "-" ) != nil)
                axlClearSelSet()
                if(view != nil then axlUIWClose(view))
                Out_File = outfile("./uni_via.log" "w")
                checked_list = nil 
                    foreach(item Input_List
                        radius_sel = air_gap_max
                        left_side = axlMXYSub(item->xy list(radius_sel -radius_sel))
                        right_side = axlMXYAdd(item->xy list(radius_sel -radius_sel))
                        ;axlDBCreateRectangle( list(left_side right_side) t "ANTI ETCH/MP5" nil)
                        axlSingleSelectBox(list(left_side right_side))
                        foreach(via setof(e axlGetSelSet() e != item && member(e checked_list)== nil && axlDistance(e->xy item->xy)< air_gap_max)
                            dis_via = axlDistance(via->xy item->xy)
                            ;printf("%L - %L\n" axlLayerGet(car(item->startEnd))->?? axlLayerGet(car(via->startEnd))->??)
                            q1 = axlLayerGet(car(item->startEnd))->nextLayer 
                            q2 = axlLayerGet(car(via->startEnd))->nextLayer 
                            ;printf("%L  %L  %L - %L\n" via->name item->name q1 q2)
                            pad_size1 = abs(caar(axlDBGetPad(item q1 "regular")->bBox))*2
                            pad_size2 = abs(caar(axlDBGetPad(via q2 "regular")->bBox))*2
                            temps = setof(e Config_via (list(pad_size1 pad_size2) == car(e)))
                            ;printf("%L\n" Config_via list(pad_size1 pad_size2) )
                            if(temps != nil then
                                re_airgap = lastelem(car(temps))
                                if(dis_via < re_airgap then
                                    if(dis_via>0 || substring(via->name 1 4) != substring(item->name 1 4) && dis_via==0 then
										;printf("%.4f %.4f %.14f " dis_via re_airgap (dis_via - re_airgap))
										acc = axlGetParam("paramDesign")->accuracy +2
										tol_chk ="00000000000000"
										round_number = atof(sprintf(nil "0.%s1"  substring(tol_chk 1 acc)))
										if(abs((dis_via - re_airgap)) > round_number then
											STT++
											fprintf(Out_File, "%n %-15.4f %-15.4f %-15L\n" STT dis_via re_airgap item->xy)
											;anti_layer = strcat("ANTI ETCH/" upperCase(car(input)))
											anti_layer = "ANTI ETCH/ALL" 
											axlVisibleLayer(anti_layer t)
											CreatePath_uni_via = axlDBCreateLine( list(item->xy via->xy), 2.0, anti_layer )
											if(caar(CreatePath_uni_via) != nil then
												axlDBAddProp(caar(CreatePath_uni_via) list( "Comment" "uni_via"))	
											)
											
											;p = axlSelectByProperty("LINES" "COMMENT")
										
										)
										
                                        
									)
                                    axlHighlightObject(item)
                                    checked_list = cons(item checked_list)
                                ) 
                            )  
                        )
                        
                    ) 
                    close(Out_File)
                    printf("Total: %n Pins Unconnect\n" STT)
                    if(STT>0 then
                        view= axlUIViewFileCreate("uni_via.log", "Show Element To Check Pins dummy" nil)   
                    ;else
                        ;deleteFile("uni_via.log")
                    )
                ) 
        )
        else
             print("Not found") 
        ) 

    ) 
;########################################################################
axlCmdRegister("mvb" `MoveViaBottom)
defun( MoveViaBottom ()
    ori_visible = axlVisibleGet()
    ori_symbols = nil
    ori_pins = nil
    input= nil
    inputs = parseString(axlEnterString( ?prompts list("Please enter your parameters :")))
        if(inputs != nil  then
            if(blankstrp(car(inputs)) == nil && atof(car(inputs)) != nil then
                    input = atof(car(inputs))
                   
            )      
        )
    axlClearSelSet()
    axlSetFindFilter(?enabled '(noall SYMBOLS PINS) ?onButtons '(noall PINS))
    mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
   while(axlSelect(?prompt "Please select an element...")
        foreach(item axlGetSelSet()
            if(item->objType=="symbol" then
                ori_symbols = cons(item ori_symbols)
            else
                ori_pins= cons(item ori_pins)
            )
        
        )	
    )
    if(ori_symbols != nil then
        foreach(symbol ori_symbols
            foreach(pin setof(e symbol->pins (e->isThrough == nil &&  member("ETCH/BOTTOM" car(e->startEnd) )!= nil) )
                ori_pins = cons(pin  ori_pins)
            )
            
        )
    )
   /* axlVisibleDesign(nil)
    axlShell("redraw")
    axlVisibleLayer("PIN" t)
    axlClearSelSet()
    axlSetFindFilter(?enabled '(noall SYMBOLS) ?onButtons '(noall SYMBOLS))
	axlAddSelectAll()
    axlVisibleLayer("PIN" nil)
    axlShell("redraw")
    axlVisibleSet(ori_visible)
    Symbols = axlGetSelSet()
    ;SymbolsAtBottoms = setof(e Symbols (car(e->pins)->isThrough == nil && member("ETCH/BOTTOM" car(e->pins)->startEnd)!= nil  ) ) */
   
    if(ori_pins == nil || input == nil then
        axlUIConfirm( "Crashed!\nTry again")
    
    else
        /*if(length(SymbolsAtBottoms) >1 then  
            axlClearSelSet()
            axlSetFindFilter(?enabled '(noall SYMBOLS) ?onButtons '(noall SYMBOLS))
            axlSingleSelectPoint()
            ori_symbol = car(axlGetSelSet())
        else
            ori_symbol = car(SymbolsAtBottoms)
                
        )
        printf("%L\n" ori_symbol->refdes)
        
        ;find via bottom
        ;find pin top, via top
        axlVisibleLayer("PIN/TOP" t)
        axlVisibleLayer("PIN/BOTTOM" t)
        */
        view = nil
        STT= 0
        padstacks_list = axlDBGetDesign()->padstacks
        modelPadstacks = nil
        moveViaList = nil
        replaceViaList = nil
        foreach(padstack padstacks_list
            modelPadstacks = cons(list(padstack->name padstack)  modelPadstacks)
        )
        Out_File = outfile("./MoveViaBottom.log" "w")
        if(length(ori_pins)>0 then
                    foreach(pin ori_pins
            axlVisibleDesign(nil)
            axlVisibleLayer("PIN/TOP" t)
            axlVisibleLayer("PIN/BOTTOM" t)
            axlClearSelSet()
            axlSetFindFilter(?enabled '(noall PINS) ?onButtons '(noall PINS))
            poly_pin = car(axlPolyFromDB(pin ?layer "ETCH/BOTTOM" ?endCapType 'ROUND ?padType 'REGULAR))
            axlAddSelectBox(poly_pin->bBox)
            sel_pins = setof(e axlGetSelSet() (member("ETCH/TOP" e->startEnd) != nil && length(e->parent->pins)>50  ) )
            pin_selected_list = nil
            list_vias = setof(e axlDBGetConnect(pin t) e->objType=="via")
            foreach(via_connected list_vias
                pin_top_list = nil
                foreach(pin_top setof(e sel_pins (memq(e pin_selected_list) == nil))
                    dis = axlDistance(via_connected->xy pin_top->xy)
                    pin_top_list = cons(list(dis pin_top)  pin_top_list)  
                )
                if(pin_top_list != nil then
                     check1 = car(sortcar(pin_top_list 'lessp))
                     if(car(check1)>0 && car(check1)< input then
                        pin_selected_list = cons(cadr(check1) pin_selected_list)
                        ;fprintf(Out_File, "1 :%-15L %-15L %-15L\n" via_connected->name  via_connected->xy cadr(check1)->xy)
                        ;axlTransformObject(via_connected ?move axlMXYSub(cadr(check1)->xy via_connected->xy))
                        if(setof(e list_vias e->xy== cadr(check1)->xy) == nil then
                            moveViaList = cons(list(via_connected cadr(check1)->xy)  moveViaList)
                        )
                        via_connected_top = setof(e axlDBGetConnect(cadr(check1) t) e->objType=="via")
                        via_connected_top_getConnects = axlDBGetConnect(car(via_connected_top) t)
                        if(length(via_connected_top_getConnects)>2 then
                            via_connected_start_par =   cadr(parseString( car(via_connected->startEnd) "/"))
                            Etchlist1 = member(via_connected_start_par axlSubclassRoute())
                            via_connected_top_start_par = cadr(parseString(cadr(car(via_connected_top)->startEnd) "/"))
                            if(member(via_connected_top_start_par Etchlist1) != nil then
                                layers = nil
                                 foreach(connect via_connected_top_getConnects
                                    if(connect->layer != nil then
                                        layers = cons(connect->layer layers )
                                    )   
                                )
                                if(length(layers)>1 then
                                    firstName = car(parseString(car(via_connected_top)->name "-"))
                                    check2 = setof(e modelPadstacks (nindex(car(e) firstName) != nil && car(cadr(e)->startEnd))=="ETCH/TOP" && car(layers) == cadr(cadr(e)->startEnd))
                                    if(check2 != nil then
                                       ;fprintf(Out_File, "%-15L %-15L %-15L\n" car(via_connected_top)->name  cadr(car(check2))->name cadr(check1)->xy)
                                       replaceViaList = cons(list(car(via_connected_top) cadr(car(check2))->name) replaceViaList)
                                    )     
                                ) 
                            )     
                        )  
                    )
                     
                )
            )   
               
        )
        
        )
        axlDBControl('dynamicFillMode  nil)
		axlShapeDynamicUpdate(nil nil)
        ;Move
        foreach(via moveViaList
            STT++
            fprintf(Out_File, "%-5n Moved %-20s %-10L %-20L\n" STT car(via)->name  car(via)->xy cadr(via))
            axlHighlightObject(car(via))
            axlTransformObject(car(via) ?move axlMXYSub(cadr(via) car(via)->xy) )
           
        )
       
        ; Replace
        foreach(via replaceViaList
            STT++
            fprintf(Out_File, "%-5n Replaced %-20s%-20s%-20L\n" STT car(via)->name cadr(via) car(via)->xy)
            axlHighlightObject(car(via))
            axlReplacePadstack(car(via) cadr(via))
            
        )
        axlDBControl('dynamicFillMode  'wysiwyg)
		axlShapeDynamicUpdate(nil nil)
        close(Out_File)
        printf("Total: %n Pins Unconnect\n" STT)
        if(STT>0 then
            view= axlUIViewFileCreate("MoveViaBottom.log", "Show Element To Check Pins dummy" nil)  
        else
            axlUIConfirm( "Not Found")
        )
		
           
    ) 
    axlVisibleSet(ori_visible)
    axlShell("redraw")    

)
;########################################################################
axlCmdRegister("MoveViaTo" `MoveViaTo)
(defun MoveViaTo ()
    view = nil
    axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	while(axlSelect(?prompt "Please select an element...") 
    Input_List = nil
    STT=0
	Input_List = axlGetSelSet()
	axlClearSelSet()
    if(view != nil then axlUIWClose(view))
	sub_info_via = car(setof(e axlVisibleGet() member("VIA CLASS" e) != nil ))->subclassinfo
	sub_vis_via = caar(setof(e sub_info_via cadr(e) == t))
				
    Out_File = outfile("./MoveViaTo.log" "w")
        foreach(item setof( e Input_List axlDBIsFixed(e->net)== nil)
            LengthVia = item->startEnd
            ;if(item->net->name =="VSS" && lindex(LengthVia "ETCH/MP27") != nil && lindex(LengthVia "ETCH/MP28") != nil then
                
				dbid_poly = car(axlPolyFromDB(item ?layer strcat("ETCH/" sub_vis_via)  ?endCapType 'NONE))
				;axlDBCreateRectangle( dbid_poly->bBox t "ANTI ETCH/ALL" )
				axlClearSelSet()
                axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall PINS))
				axlAddSelectBox(dbid_poly->bBox )
                if(axlGetSelSet() != nil && axlDBIsFixed(item) == nil then
                    xyPin = car(axlGetSelSet())->xy
                    if(xyPin != item->xy then
                        fprintf(Out_File, "%-15L %-15L %-15L\n" item->xy xyPin axlMXYSub(xyPin item->xy) )
                        STT++
                        axlTransformObject(item ?move axlMXYSub(xyPin item->xy)) 
                    )
                )
                
            ;) 
        ) 
        close(Out_File)
        axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
        printf("Total: %n Pins Unconnect\n" STT)
		view= axlUIViewFileCreate("MoveViaTo.log", "Show Element To Check Pins dummy" nil)        
    
    )


)
;########################################################################
 axlCmdRegister("via_cap_chk" `via_cap_chk)
(defun via_cap_chk ()
	view = nil
    ori_visible = axlVisibleGet()
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	while(axlSelect(?prompt "Please select an element...")
	if(view != nil then axlUIWClose(view))
	STT=0
	Input_List = nil
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./via_cap_chk.log" "w")
			foreach(item_via Input_List	
                via_net_name = item_via->net->name
				if(via_net_name != "VSS" then
					connect_chk = setof(e axlDBGetConnect(item_via t) (e->objType =="shape"))
					shape_net = setof(e axlDBGetShapes("ETCH") (e->net->name == via_net_name && nindex(e->layer "PA") != nil))
                    if(length(connect_chk) < length(shape_net) then
                        foreach(out_item shape_net
                            if(memq(out_item connect_chk)== nil then
                               layer_at = cadr(parseString(out_item->layer "/"))
                               via_layer_at_1 = cadr(parseString(car(item_via->startEnd) "/"))
                               via_layer_at_2 = cadr(parseString(cadr(item_via->startEnd) "/"))
                               
                               if(member(layer_at member(via_layer_at_2  axlSubclassRoute(?field 'isEtch))) == nil || layer_at == via_layer_at_2 then
                                    STT++
                                    fprintf(Out_File, "%-5n %-10s %-10s %-10L\n" STT via_net_name out_item->layer item_via->xy ) 
                               
                               )
                                
                            
                            )
                        )
                         
                    
                    )
                   
                    
				)
					
				)
			close(Out_File)
            axlVisibleSet(ori_visible)
			printf("Total: %n Pins Unconnect\n" STT)
			
			if(STT == 0 then
			 axlUIConfirm( "Finished!")
			 else
			 view= axlUIViewFileCreate("via_cap_chk.log", "Show Element To Check Pins dummy" nil)
			)
			else
				printf("No Element found!\n")
					
		)
	)
)
;########################################################################
   axlCmdRegister("tiber" `tiber) 
   (defun tiber ()
    STT = 0
    Out_File = outfile("./tiber.log" "w")
    NetDesignList = setof(element axlDBGetDesign()->nets nindex(element->name "_SLD") != nil)
    results = nil
    foreach(item NetDesignList
        foreach(branch item->branches
            v1 = setof(e branch->children (e->objType =="via" &&  e->name  == "TIBER_CUT") )
            v2 =  setof(e branch->children (e->objType =="via" &&  e->name == "TIBER_2") )
            pins =  setof(e branch->children (e->objType =="pin") )
            if(length(v1) == 1 && length(v2) == 2  then 
            else  
               if(length(v1) != 2 && length(v2) != 4  then 
                results = cons( list(item->name car(pins)->xy) results)
                )
            )    
       )
    )
    foreach(item unique(results)
         STT++
         fprintf(Out_File, "2 :%-5n %-20s %-10L\n" STT  car(item) cadr(item) )
    
    )
    close(Out_File)
	printf("Total: %n Pins Unconnect\n" STT)
        if(STT == 0 then
            axlUIConfirm( "Finished!")
        else
            view= axlUIViewFileCreate("tiber.log", "Show Element To Check Pins dummy" nil)
        )
   )
;########################################################################
   axlCmdRegister("vtiber" `vtiber) 
   (defun vtiber ()
    ori_visible = axlVisibleGet()
    STT = 0
    Out_File = outfile("./vtiber.log" "w")
    axlClearSelSet()						
	axlSetFindFilter(?enabled '(noall  VIAS NAMEFORM) ?onButtons '(noall VIAS))
    axlAddSelectAll()
    foreach(item  setof(element axlGetSelSet() element->name =="TIBER_CUT")
        axlClearSelSet()						
		axlSetFindFilter(?enabled '(noall  SHAPES NAMEFORM) ?onButtons '(noall SHAPES))
        axlVisibleLayer("ROUTE KEEPOUT/BL" t)
		axlSingleSelectPoint(item->xy)
       if(setof(e axlGetSelSet() (index(e->layer "ROUTE KEEPOUT") != nil)) == nil then
            STT++
            fprintf(Out_File, "%-5n %-10L\n" STT  item->xy ) 
        )
        
    )
    axlVisibleSet(ori_visible)
    close(Out_File)
	printf("Total: %n Pins Unconnect\n" STT)
        if(STT == 0 then
            axlUIConfirm( "Finished!")
        else
            view= axlUIViewFileCreate("vtiber.log", "Show Element To Check Pins dummy" nil)
        )
   )
;########################################################################
axlCmdRegister("conn_via_counter" `conn_via_counter)
(defun conn_via_counter ()
	view = nil
	shape_list = nil
	limit_layer = reverse(member("2PA" reverse(axlSubclassRoute(?field 'isEtch))))
	foreach(layer limit_layer
		s_l = nil
		foreach(shape axlDBGetShapes(strcat("ETCH/" layer))
				s_l = cons(shape->net->name s_l)
		)
		foreach(net unique(s_l)
			shape_list = cons(net shape_list)	
		)
	)
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	while(axlSelect(?prompt "Please select an element...")
	if(view != nil then axlUIWClose(view))
	STT=0
	Input_List = nil
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./conn_via_counter.log" "w")
			foreach(item_via Input_List	
				if(item_via->net->name != "GND" then
					connect_chk = setof(e axlDBGetConnect(item_via t) (e->objType =="path"))
					st = cadr(parseString(car(item_via->startEnd) "/"))
					end = cadr(parseString(cadr(item_via->startEnd) "/"))
					
					if(connect_chk == nil && member(st limit_layer) != nil && member(end limit_layer) != nil then
						connect_shape = length(setof(e axlDBGetConnect(item_via t) (e->objType =="shape")))
						connect_request = length(setof(e shape_list (e == item_via->net->name) ))
						if(connect_shape != connect_request   then	
							axlHighlightObject(item_via)
							STT++
							fprintf(Out_File, "%-5n %-10L %-10s %-10L\n" STT list( connect_shape  connect_request)  item_via->name item_via->xy)
							
						)
					)
					
				)
					
				)
			close(Out_File)
			printf("Total: %n Pins Unconnect\n" STT)
			
			if(STT == 0 then
			 axlUIConfirm( "Finished!")
			 else
			 view= axlUIViewFileCreate("conn_via_counter.log", "Show Element To Check Pins dummy" nil)
			)
			else
				printf("No Element found!\n")
					
		)
	)
)
;########################################################################
axlCmdRegister("check_via_between_pair" `check_via_between_pair)
	( defun check_via_between_pair ()
	let(( (view nil) (STT 0) )
		axlClearSelSet()
		axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
		mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Next" 'antena_via_cline)))
		axlUIPopupSet(mypopup)	
		view = nil
		while(axlSelect(?prompt "Please select an element...") 
		if(view != nil then axlUIWClose(view))
		Input_List = nil
		STT = 0
		Input_List = axlGetSelSet()
			Out_File = outfile("./check_via_between_pair.log" "w")
			foreach(item_via Input_List 
				poly_via = car(axlPolyFromDB(item_via ?layer car(item_via->startEnd) ?endCapType 'ROUND ?padType 'REGULAR))
				Item_poly_Expand = car(axlPolyExpand(poly_via 155.0 'NONE))	
				;printf("%L\n" list(Item_poly_Expand_1 Item_poly_Expand_2 axlCNSGetSpacing("PAIR" car(item_via->startEnd) 'via_line)))
				;axlDBCreateShape(Item_poly_Expand_1 t "ANTI Etch/MP12"  nil)
				axlClearSelSet()						
				axlSetFindFilter(?enabled '(noall  CLINES NAMEFORM) ?onButtons '(noall CLINES))
				axlAddSelectBox(Item_poly_Expand->bBox)
				se_list = setof( e axlGetSelSet() (e->net->diffpair != nil && e->net->name != item_via->net->name )) 
				if(length(se_list)>1 then	
					temp = nil
					foreach(item se_list
						member_pair = item->net->diffpair->groupMembers
						foreach(item_1 se_list
							if(memq(item_1->net member_pair) != nil then
								temp = cons(item_1 temp)
							)
						)
					)
					;printf("%L\n" temp)
					if(length(temp)>3 then
						STT++
						fprintf(Out_File, "%-5n %-10s %-10s %-10L\n" STT strcat("ETCH/" cadr(parseString(car(se_list)->layer "/")) ) item_via->name item_via->xy)
						axlDBCreateExternalDRC( "Undefine", item_via->xy
						strcat("DRC ERROR CLASS/" cadr(parseString(car(se_list)->layer "/")) ), nil , nil , "None")
					)
					
					
				)
						
				
			)
			axlSetFindFilter(?enabled '(noall VIAS ) ?onButtons '(noall VIAS))
			close(Out_File)
			if(STT == 0 then
			 axlUIConfirm( "Finished!")
			 else
			 view = axlUIViewFileCreate("check_via_between_pair.log", " " nil)
			)
		
		)
	)		
	
)

;########################################################################
axlCmdRegister("fvd" `find_via_darling)
	( defun find_via_darling ()
		axlClearSelSet()
		axlSetFindFilter(?enabled '(noall VIAS ) ?onButtons '(noall VIAS))
		mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Next" 'antena_via_cline)))
		axlUIPopupSet(mypopup)	
		view = nil
		while(axlSelect(?prompt "Please select an element...") 
		if(view != nil then axlUIWClose(view))
		Input_List = nil
		STT = 0
		Input_List = axlGetSelSet()
			Out_File = outfile("./find_via_darling.log" "w")
			foreach(item_via Input_List 
				length_conn_via = length(axlDBGetConnect(item_via t))
				foreach(item_get_connect setof(e axlDBGetConnect(item_via t) (e->objType=="shape"))
					if(length(axlDBGetConnect(item_get_connect t)) <=1 then
						if(length_conn_via <=2 then
							STT++
							fprintf(Out_File, "%-5n %-10s %-10s %-10L\n" STT item_get_connect->layer item_via->name item_via->xy)
						)
					)	
				)
			)
			close(Out_File)
			view = axlUIViewFileCreate("find_via_darling.log", "Show Element To Check Pins dummy" nil)
		
		)
	
	)
;########################################################################
axlCmdRegister("gvcnn" `getviaconnect)
(defun getviaconnect ()
	let( ( (view nil) (stt 0) ( sel_list nil ) (layer_list nil) ) 
		axlClearSelSet()
		axlSetFindFilter(?enabled '(noall VIAS CLINES SHAPES NAMEFORM) ?onButtons '(noall VIAS CLINES SHAPES))
		mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Next" 'antena_via_cline)))
		axlUIPopupSet(mypopup)	
		layer1 ="MP8"
		layer2 ="MP12"
		;printf("L\n" member(layer1 axlSubclassRoute(?field 'isEtch)))
		foreach(item  axlSubclassRoute(?field 'isEtch)
			if(item == layer1 then
				layer_list_1 = member(layer1 axlSubclassRoute(?field 'isEtch))
				layer_list_2 = reverse(layer_list_1)
				if(layer_list == nil then
				layer_list = reverse(member(layer2 layer_list_2))
				)
			else
				if(item == layer2 then
				layer_list_1 = member(layer2 axlSubclassRoute(?field 'isEtch))
				layer_list_2 = reverse(layer_list_1)
				if(layer_list == nil then
				layer_list = reverse(member(layer1 layer_list_2))
				)
				)
			)
		)
		;printf("%L\n" layer_list)
		while(axlSelect(?prompt "Please select an element...")
			if(view != nil then axlUIWClose(view))
			sel_list = nil
			stt=0
			sel_list = axlGetSelSet()
			Out_File = outfile("./getviaconnect.log" "w")
			foreach(item  sel_list
				foreach(pad_item  axlDBGetConnect(item t)  
					if(pad_item->objType=="via"|| pad_item->objType=="pin" then
						stt++
						fprintf(Out_File, "%-5n %-10L\n" stt pad_item->??)
					)
			
				)
			)
		close(Out_File)
		view = axlUIViewFileCreate("getviaconnect.log", "Show Element To Check Pins dummy" nil)
		)	
		
	)
	
)


;########################################################################
axlCmdRegister("avc" `avc)
( defun avc ()
    view = nil
    yesno = axlUIYesNo( "Do you want to replace ?" nil 'no )
    pad_list = nil
    foreach( padstack axlDBGetDesign()->padstacks
        pad_list = cons(list(padstack->startEnd padstack) pad_list)
    )
    yesno2 = nil
    axlSetFindFilter(?enabled '(noall SHAPES VIAS CLINES NAMEFORM) ?onButtons '( noall VIAS CLINES))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
    while(axlSelect(?prompt "Please select an element...")
	if( view != nil then axlUIWClose(view) )
	Input_List = nil
	STT=0
	Input_List = axlGetSelSet()
	axlClearSelSet()
        Out_File = outfile("./avc.log" "w")
        out_list = nil
		foreach(item setof(e Input_List axlDBIsFixed(e->net) == nil)
            conn_shape_data = nil
            if(item->objType =="via" then
              conn_shape_data = list(item)
            else
              conn_shape_data = setof(e axlDBGetConnect(item t) (e->objType=="via" && axlDBIsFixed(e) == nil) )
            )
            if(length(conn_shape_data)>0 then
                foreach(via conn_shape_data
                    if(axlDBGetConnect(via t) == nil then
                        if(yesno2 == nil then
                            yesno2 = axlUIYesNo( "Do you want to delete ?" nil 'no )
                        )
                        if( yesno2 == t && axlDBIsFixed(via) == nil then
                            axlDeleteObject(via)
                        )
                    else
                        objType_list = nil
                        objType_list2 = nil
                        objType_list3 = nil
                        conn_pin = setof(e axlDBGetConnect(via t) e->objType=="pin" )
                        conn_pin2 = nil
                        foreach(p conn_pin 
                            conn_pin2 = cons(car(p->startEnd) conn_pin2)
                        )
                        foreach(conn_via setof(e axlDBGetConnect(via t) e->objType !="pin" && e->objType !="via" )
                          objType_list2 = cons(conn_via->layer objType_list2)  
                        )
                        foreach(conn_via setof(e axlDBGetConnect(via t)  e->objType =="via" )
                           if(member(car(via->startEnd) conn_via->startEnd) != nil then
                            objType_list3 = cons(car(via->startEnd) objType_list3)  
                           else
                            objType_list3 = cons(cadr(via->startEnd) objType_list3) 
                           )  
                        )
                        foreach(sub axlSubclassRoute(?field 'isEtch)
                            isEtch = strcat("ETCH/" sub)
                            if(member(isEtch conn_pin2) != nil || member(isEtch objType_list2) != nil || member(isEtch objType_list3) != nil then
                               objType_list = cons(isEtch objType_list) 
                            )
                        )
                        objType_list = reverse(objType_list)
                        name_via = nil
                        if(length(objType_list) ==1 then
                            out_list = cons(list("dangling via " via->name via->xy) out_list)
							;axlDBAddProp( via list( "Comment" "antena_via_cline"))
                        else
                            name_temp = unique(list(car(objType_list) lastelem(objType_list)))   
                            if( name_temp != via->startEnd then
                                find_pad = setof(e pad_list (car(e)== name_temp))
                                    if(length(find_pad)==1 then
                                        name_via = cadr(car(find_pad))->name
                                    else
                                        if(length(find_pad)==0 then
                                            name_via = ", No found Via in database"
                                        else
                                            chk1 = axlDBGetPad(via car(name_temp) "ANTI")
                                            chk2= axlDBGetPad(via cadr(name_temp) "ANTI")
                                            ;printf("%L\n" name_temp)
                                            foreach(pad find_pad
                                               chk_1 = axlDBGetPad(cadr(pad) caar(pad) "ANTI")
                                               chk_2 = axlDBGetPad(cadr(pad) cadr(car(pad)) "ANTI")
                                               chk3 = axlDBGetPad(via car(name_temp) "REGULAR")
                                               chk4 = axlDBGetPad(via cadr(name_temp) "REGULAR")
											   chk_3 = axlDBGetPad(cadr(pad) car(name_temp) "REGULAR")
											   chk_4 = axlDBGetPad(cadr(pad) cadr(name_temp) "REGULAR")
                                               if( list(chk3->bBox chk4->bBox) == list(chk_3->bBox chk_4->bBox) && list(chk_1->bBox chk_2->bBox) == list(chk1->bBox chk2->bBox) && axlLoadPadstack(via->name)->drillDiameter == axlLoadPadstack(cadr(pad)->name)->drillDiameter then
													name_via = cadr(pad)->name
												 
                                               )
                                            )
                                        )
                                   )
                                 if(name_via== nil then
                                   STT++           
                                   fprintf(Out_File "%-5n %-21s %-15s %-15L\n" STT "No found database for " via->name via->xy)
                                else
                                    if(index(name_via "No found") == nil && yesno== t then 
                                        out_list = cons(list(strcat("replaced successfully " name_via) via->name via->xy) out_list)
                                        axlReplacePadstack(via name_via)
                                    else
                                        out_list = cons(list(strcat("antenna via " name_via) via->name via->xy) out_list)
                                    ) 
                                )
                                 
                            )
                        )
                    )
                )
            )
        )
        if(out_list != nil then
            foreach(row unique(sortcar(out_list nil)) 
                STT++
                fprintf(Out_File "%-5n %-15s %-20s %-15L\n"  STT cadr(row) car(row) caddr(row))
            
            )  
        )    
		close(Out_File)
		printf("Total: %n Cline checked by in skill program\n" STT)
        if(STT>0 then
            view= axlUIViewFileCreate("avc.log", "Show Element To Check Cline" nil)
        else
             axlUIConfirm( "Not Found")
             deleteFile("get_shape_info.log")
        )
						
     )		
		
)

;########################################################################
/*axlCmdRegister("avc" `antena_via_cline)
(defun replace_via (via_select)
		layer_list = nil
				via_name = nil
				via_list = nil
				via_name_rex_end = nil
				via_name_str_end = nil
				via_name_rex_start = nil
				via_name_str_start = nil
				foreach(layer_item axlSubclassRoute(?field 'isEtch)
					;|| member(car(via_select->startEnd) e->startEnd) != nil && e->objType =="via" )
					if(setof(e axlDBGetConnect(via_select t) (e->layer == strcat("ETCH/" layer_item)
					|| strcat("ETCH/" layer_item) == car(e->startEnd)&& strcat("ETCH/" layer_item) == cadr(via_select->startEnd)
					|| strcat("ETCH/" layer_item) == cadr(e->startEnd)&& strcat("ETCH/" layer_item) == car(via_select->startEnd) 
					;|| member(strcat("ETCH/" layer_item) e->startEnd) != nil && e->objType =="pin"
					)
					) != nil then 
						layer_list = cons(strcat("ETCH/" layer_item)  layer_list)	
					)
				)
				if(length(layer_list) >= 2 then
					via_select_ST = via_select->startEnd
				if(member(car(via_select_ST)  layer_list) != nil || member(cadr(via_select_ST)  layer_list) != nil then
					if(member(car(via_select_ST)  layer_list) != nil then 
					via_list = setof( e axlGetAllViaList() (car(layer_list) == cadr(e->startEnd) && car(via_select_ST)== car(e->startEnd)))
					via_name_rex_end = cadr(parseString(cadr(via_select_ST) "/"))
					via_name_str_end = cadr(parseString(car(layer_list) "/"))					
				else
						if(member(cadr(via_select_ST)  layer_list) != nil then 
							via_list = setof( e axlGetAllViaList() (cadr(via_select_ST) == cadr(e->startEnd) && car(reverse(layer_list)) == car(e->startEnd)))		
							
							via_name_rex_start = cadr(parseString(car(via_select_ST) "/"))
							via_name_str_start = cadr(parseString(car(reverse(layer_list)) "/"))	
						)		
				)
				else
					if( setof(e axlDBGetConnect(via_select t) (e->layer == car(via_select_ST) || member(car(via_select_ST) e->startEnd))) == nil then
						via_name_rex_end = cadr(parseString(cadr(via_select_ST) "/"))
						via_name_str_end = cadr(parseString(car(layer_list) "/"))
					)
					if( setof(e axlDBGetConnect(via_select t) (e->layer == cadr(via_select_ST) || member(cadr(via_select_ST) e->startEnd))) == nil then
						via_name_rex_start = cadr(parseString(car(via_select_ST) "/"))
						via_name_str_start = cadr(parseString(car(reverse(layer_list)) "/"))
						
					)
				
				)
				via_list_1 = nil
				via_list_2 = nil
				if(via_name_rex_end != nil then
					rexCompile(via_name_rex_end)
					via_name = rexReplace(via_select->name via_name_str_end 1)	
				else
					via_name = via_select->name
				)
				if(via_name_rex_start != nil then
					rexCompile(via_name_rex_start)
					via_name = rexReplace(via_name via_name_str_start 1)
					;via_list_2 = setof( e axlGetAllViaList() (strcat("ETCH/" via_name_str_start)== car(e->startEnd)))
				)
				if(via_name_rex_end != nil then
					via_list_1 = setof( e axlGetAllViaList() (strcat("ETCH/" via_name_str_end) == cadr(e->startEnd) && car(layer_list) == cadr(e->startEnd)))
				
				else
					via_list_1 = setof( e axlGetAllViaList() (strcat("ETCH/" via_name_str_start)== car(e->startEnd) && car(reverse(layer_list)) == cadr(e->startEnd)))
				
				)
				
					if(via_list != nil || via_list_1 != nil then
						via_selected_list = cons(via_select via_selected_list)
						if(via_select->name != via_name then
							if(setof(e via_list (e->name == via_name)) != nil || setof(e via_list_1 (e->name == via_name)) != nil then
								if(yesno == t then
									;dem++
									;fprintf(Out_File, "%-5n %-15s %-15s %-15L \n",dem via_select->name via_name via_select->xy)
									ok_list = cons(list(via_select->name via_name via_select->xy) ok_list)
										axlReplacePadstack(via_select via_name)

								else
									unreplace_list = cons(list(via_select->name via_name via_select->xy) unreplace_list)
									;fprintf(Out_File, "%-5n %-15s %-15s %-15L exist Antena Via!\n",dem via_select->name via_name via_select->xy)
								)	
							
							else
								via_list_chk = nil
								if(setof(e via_list_1 (e->name == via_name)) == nil then
									if(via_list_1 != nil then 
										via_list_chk = via_list_1
									)
								else
									if(via_list != nil then 
										via_list_chk = via_list
										
										
									)
								)
								if(via_list_chk != nil then
									foreach(item via_list_chk
										d_via_st = axlDBGetPad(item car(reverse(layer_list)) "regular")->bBox
										h_st = axlDistance(car(d_via_st) cadr(d_via_st))
										d_via_or_st = axlDBGetPad(via_select car(reverse(layer_list)) "regular")->bBox
										or_st = axlDistance(car(d_via_or_st) cadr(d_via_or_st))
										d_via_end = axlDBGetPad(item car(layer_list) "regular")->bBox
										h_end = axlDistance(car(d_via_end) cadr(d_via_end))
										d_via_or_end = axlDBGetPad(via_select car(layer_list) "regular")->bBox
										or_end = axlDistance(car(d_via_or_end) cadr(d_via_or_end))
											if(abs(h_st - or_st) <= 0.001 && abs(h_end- or_end) <= 0.001 then
												if(yesno == t then
													;dem++
													;fprintf(Out_File, "%-5n %-15s %-15s %-15L \n",dem via_select->name item->name via_select->xy)
													ok_list = cons(list(via_select->name item->name via_select->xy) ok_list)
													axlReplacePadstack(via_select item->name)
												else
												unreplace_list = cons(list(via_select->name item->name via_select->xy) unreplace_list)
												;fprintf(Out_File, "%-5n %-15s %-15s %-15L exist Antena Via!\n",dem via_select->name via_name via_select->xy)
													)
											)
										)
									)
							)
						
						)
						
						
					else
						fail_list = cons(list(via_select->name via_select->xy) fail_list)
						;fprintf(Out_File, "%-5n %-15s %-15L Unsuccessful !\n",dem via_select->name via_select->xy)
							;axlHighlightObject(cline_item)	
					)
				
				else
					fail_list = cons(list(via_select->name via_select->xy) fail_list)
					;fprintf(Out_File, "%-5n %-15s %-15L Unsuccessful !\n",dem via_select->name via_select->xy)
					;axlHighlightObject(cline_item)	
				
				)			

)
(defun antena_via_cline ()
	yesno = axlUIYesNo( " Would you like to replace ?" "Skill Show Antena Via" )
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall VIAS CLINES NAMEFORM) ?onButtons '(noall VIAS CLINES))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Next" 'antena_via_cline)))
	axlUIPopupSet( mypopup)
	Input_List = nil
	view = nil
	while(axlSelect(?prompt "Please select an element...")
		dem=0
		Input_List = nil
		via_selected_list = nil 
		name_via_name = nil
		unreplace_list = nil
		fail_list  = nil
		ok_list  = nil
		fixed_list  = nil
		if( view != nil then axlUIWClose(view))
		Input_List = axlGetSelSet()
		axlClearSelSet()
		Out_File = outfile("./antena_via_cline.log" "w")
		foreach(cline_item Input_List	
			if(cline_item->objType == "via" && memq(cline_item via_selected_list) == nil then
				if(axlDBIsFixed(cline_item) != nil then
					fixed_list = cons(list(cline_item->name cline_item->xy) fixed_list)
				else
					replace_via(cline_item)
				)
			else
				foreach(via_item setof( e axlDBGetConnect(cline_item t) (e->objType =="via")  )
					if(axlDBIsFixed(via_item) != nil then
						fixed_list = cons(list(via_item->name via_item->xy) fixed_list)
					else
						replace_via(via_item)	
					)
						
				)	
			)	
		)
		if(unreplace_list == nil
		 && fail_list  == nil
		 && ok_list  == nil
		 && fixed_list  == nil then
		 axlUIConfirm( "No Found Error!")
		 else
			
			 if(unreplace_list != nil
				foreach( item unique(unreplace_list)
					dem++
					fprintf(Out_File, "%-5n %-15s %-15s %-15L exist Antena Via!\n",dem car(item) cadr(item) lastelem(item))
				)
			 )
			 if(fail_list != nil
				foreach( item  unique(fail_list)
					dem++	
					fprintf(Out_File, "%-5n %-15s %-15L Unsuccessful!\n",dem car(item) cadr(item))
				)
			 )
			  if(fixed_list != nil
				foreach( item  unique(fixed_list)
					dem++
					fprintf(Out_File, "%-5n %-15s %-15L fixed Via!\n",dem car(item) cadr(item))
				)
			 )
			 if(ok_list != nil
				foreach( item  unique(ok_list)
					dem++
					fprintf(Out_File, "%-5n %-15s %-15s %-15L replaced Via successful !\n",dem car(item) cadr(item) lastelem(item))
				)
			 )
			 
			close(Out_File)
		printf("Total: %n Pins Unconnect\n" dem)
		
		view = axlUIViewFileCreate("antena_via_cline.log", "Show Element To Check Pins dummy" nil)
		)
		
	)
) */
;########################################################################
axlCmdRegister("assign_color" `assign_color)
(defun assign_color ()
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	axlSingleSelectPoint()
	printf("%L\n" car(axlGetSelSet())->prop->??)

)

;########################################################################
axlCmdRegister("getvialist" `getvialist)
(defun getvialist ()
	dem=0
	view= nil
	Out_File = outfile("./via_name_area.log" "w")
			netNames = axlDBGetDesign()->nets
			
			vias = nil
			foreach( item netNames
				foreach(branch item ->branches
			
				foreach(child branch ->children
				
					when(child ->objType == "via"
					dem++
						fprintf(Out_File, "%-5n %-15L\n",dem, child->??)
						vias = cons(child, vias)
					)
				)
			
			)
			
			)
			
			;foreach(item vias
				;dem++
				;fprintf(Out_File, "%-5n %-15s %-15L %-15L\n",dem, item->name, item->startEnd)
							
			;)
	close(Out_File)
	printf("Total: %n Pins Unconnect\n" dem)
	view= axlUIViewFileCreate("via_name_area.log", "Show Element To Check Pins dummy" nil)					
)
;########################################################################
axlCmdRegister("rsv" `via_name_area)
input_rsv2 = nil
input_rsv = nil
(defun rsv_ip_default_clear () 
	input_rsv = nil
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Set Size" 'rsv_ip_default)))
	axlShell("done")
	axlShellPost("rsv")
)
(defun rsv_ip_default () 
	if( input_rsv2 != nil then
			input_rsv2 = axlUIPrompt("Enter Size Via :" input_rsv2 )
	else
		input_rsv2 = axlUIPrompt("Enter Size Via:")
	)
	if(input_rsv2 != nil then input_rsv_via_name_area = input_rsv2 else input_rsv2 = input_rsv_via_name_area)
	if(input_rsv_via_name_area != nil && atof(car(parseString(input_rsv_via_name_area " "))) != nil then 
		if(setof(e axlDBGetDesign()->padstacks~>name (index(e strcat("VIA" input_rsv_via_name_area)) != nil)) == nil then
			axlUIConfirm( "Not Found")
		else
			input_rsv = car(parseString(input_rsv_via_name_area " "))
			printf("%L\n" input_rsv)
			mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Reset" 'rsv_ip_default_clear)))
			axlShell("done")
			axlShellPost("rsv")
		)
		
	else
		axlUIConfirm( "Enter parameters of program" 'error )
	)
)
(defun via_name_area ()
	let( ( (view nil) (STT 0) (input_rsv_via_name_area nil) )
     axlUIWCloseAll()
		sub_list = nil
		foreach(sub axlSubclassRoute()
			sub_list = cons(strcat("ETCH/" sub) sub_list)
		)
		sub_list = reverse(sub_list)
		axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
		if(input_rsv != nil then
		mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Reset" 'rsv_ip_default_clear)))
		else
		mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Set size" 'rsv_ip_default)))
		)
		axlUIPopupSet( mypopup)
			while(axlSelect(?prompt "Please select an element...")
				if( view != nil then axlUIWClose(view))
				STT=0
				input_rsv_List = nil
				input_rsv_List = axlGetSelSet()
				;rsv_GetAllViaList =  axlGetAllViaList()
				if(length(input_rsv_List) != 0 then
					Out_File = outfile("./via_name_area.log" "w")
					foreach(item input_rsv_List
						if( axlDBIsFixed(item) == nil then
							top_layer = cadr(member(car(item->startEnd) sub_list))
							d_via_st = axlDBGetPad(item top_layer "regular")->bBox
							bottom_layer = cadr(member(cadr(item->startEnd) reverse(sub_list)))
							d_via_end = axlDBGetPad(item bottom_layer "regular")->bBox
							h_st = axlDistance(car(d_via_st) cadr(d_via_st))
							h_end = axlDistance(car(d_via_end) cadr(d_via_end))
							;printf("%L\n" list( input_rsv h_st h_end ) )
							if(input_rsv == nil then
								if(index(item->name "VIA50") != nil then
									input_rsv_size = "75"
								else
									input_rsv_size = "50"
								)
							else
								input_rsv_size = input_rsv
							)
							tmp = parseString(item->name "")
							num_tmp = nil
							while(length(tmp) >0 
								st_item = car(tmp)
								if(atoi(st_item) == nil then
									if( num_tmp != nil then
										tmp = nil
									)
								else
									num_tmp = cons(st_item num_tmp)
								)
								tmp = cdr(tmp )
							)
							if(index(item->name "-") != nil then
								_size_via = buildString(reverse(num_tmp) "")
								_new_size_via = input_rsv_size
								old_size = car(parseString(item->name "-"))
								rexCompile(_size_via)
								name_via = strcat(rexReplace(old_size _new_size_via 1)	 "-" buildString(cdr(parseString(item->name "-")) "-"))
								if(axlLoadPadstack(name_via) != nil then
									STT++
									fprintf(Out_File, "%-5n %-20s %-20s %-20L\n", STT item->name name_via  item->xy)
									axlReplacePadstack(item name_via)
								)
							)
						
							/*if(h_st / sqrt(2) != input_rsv_size && h_end / sqrt(2) != input_rsv_size then
								;printf("%L\n" list(car(item->startEnd) top_layer cadr(item->startEnd) bottom_layer) )
								;size_via_chk(item)
								find_via_list = setof(e rsv_GetAllViaList ( e->startEnd == item->startEnd ))
								if( length(find_via_list) >=2 then
									foreach(via_item find_via_list
											_d_via_st = axlDBGetPad(via_item top_layer "regular")->bBox
											_d_via_end = axlDBGetPad(via_item bottom_layer "regular")->bBox
											_h_st = axlDistance(car(_d_via_st) cadr(_d_via_st))
											_h_end = axlDistance(car(_d_via_end) cadr(_d_via_end))
											if(_h_st / sqrt(2) == input_rsv_size || _h_end / sqrt(2) == input_rsv_size then
												STT++
												fprintf(Out_File, "%-5n %-20s %-20s %-20L\n", STT item->name via_item->name  item->xy)
												axlReplacePadstack(item via_item->name)
											)	
									)
								else
									STT++
									fprintf(Out_File, "%-5n %-20s %-20L %-20L Error !\n", STT item->name, length(find_via_list), item->xy)
								)  
							)*/
						else
						   
							printf("%s fixed replace no successfully \n" item->name)
						
						)
								
					)
					fprintf(Out_File, "%s\n", "-------------------------------------------------")
					fprintf(Out_File, "Issues exists in your design : %n Pins\n", STT)
					fprintf(Out_File, "%s\n", "-------------------------------------------------")
					close(Out_File)
					printf("Total: %n Pins Unconnect\n" STT)
					; if(STT == 0 then
					 ; ;axlUIConfirm( "Finished!")
					; else
					; view = axlUIViewFileCreate("via_name_area.log", "Show Element To Check Pins dummy" nil)
					; )
				else
				
					printf("No Element found!\n")
									
				)
			)
		
	)
	
)
(defun size_via_chk (via)
	d_via_st_ = axlDBGetPad(via car(via->startEnd) "regular")->bBox
	d_via_end_ = axlDBGetPad(via cadr(via->startEnd) "regular")->bBox
	h_st_ = axlDistance(car(d_via_st_) cadr(d_via_st_))
	h_end_ = axlDistance(car(d_via_end_) cadr(d_via_end_))
	

)
;########################################################################
axlCmdRegister("adjust_via_bottom" `adjust_via_bottom)
(defun adjust_via_bottom ()
		axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall PINS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	view = nil
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	dem=0
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./adjust_via_bottom.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-4s\n", "STT: ","NET :" "Location :")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "pin" then
							axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS ))
								get_db_all = axlGetSelSet(axlAddSelectBox(item->bBox))		
								axlClearSelSet()
									if( length(get_db_all) ==4 then
										xy_pin = item->xy
										name_via = car(get_db_all)->name
										axlDBCreateVia(name_via, axlMXYAdd(xy_pin 112.5:112.5), car(get_db_all)->net->name, nil , nil )
										axlDBCreateVia(name_via, axlMXYAdd(xy_pin -112.5:112.5), car(get_db_all)->net->name, nil , nil )
										axlDBCreateVia(name_via, axlMXYAdd(xy_pin 112.5:-112.5), car(get_db_all)->net->name, nil , nil )
										axlDBCreateVia(name_via, axlMXYAdd(xy_pin -112.5:-112.5), car(get_db_all)->net->name, nil , nil )
										
										;via_new = axlDBCreateVia(input_via->name,item->xy,item->net,nil,nil)
									;fprintf(Out_File, "%-20L %-20L\n", name_via, item->xy)
									foreach( temp get_db_all	
										axlDeleteObject(temp)
								)
									)
								
								
							)		
						
						)
						axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall PINS))
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						dem=0
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem) 
						view= axlUIViewFileCreate("adjust_via_bottom.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("get_via_name" `get_via_name)
(defun get_via_name ()
input = parseString(upperCase(axlEnterString( ?prompts list("Please enter your layer :"))))
printf("%-10L %-10L\n" strcat("ETCH/" car(input)) strcat("ETCH/" cadr(input)))
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	temp_lst= list()
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	dem=0
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./get_via_name.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-30s %-4s\n", "STT: ","NET :" "VIA : " "Location :")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" then		
							if(car(item->startEnd) == strcat("ETCH/" car(input)) && cadr(item->startEnd) == strcat("ETCH/" cadr(input)) then
									dem=dem +1
									axlDBAddProp(item list( "Comment" cadr(input)))
									fprintf(Out_File, "pick  %L\n", item->xy)	
							)	
						)		
						
				)
				close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view= axlUIViewFileCreate("get_via_name.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("via_name_chk" `via_name_chk)
(defun via_name_chk ()
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	temp_lst= list()
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	dem=0
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./via_name_chk.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-30s %-4s\n", "STT: ","NET :" "VIA : " "Location :")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" then
									axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall all ))
									get_db_all= axlGetSelSet(axlSingleSelectPoint(item->xy))	
									axlClearSelSet()
									foreach(temp get_db_all
										if(temp->name != item->name then
									temp_lst= cons(list(temp->net->name temp->xy) temp_lst)
										)									
									)
									if(member(list(item->net->name item->xy) temp_lst) != nil && length(parseString(item->net->name "_"))>1 then
									 dem=dem +1
									fprintf(Out_File, "%-5n %-15s %-20s %-10L\n",dem,item->net->name,item->name,item->xy)	
									)
									temp_lst= nil
								;if(car(item->startEnd) != "ETCH/TOP" && cadr(item->startEnd) !="ETCH/BOTTOM" then
									;dem=dem +1
									;fprintf(Out_File, "%-5n %-15s %-20s %-10L\n",dem,item->net->name,item->name,item->xy)	
								;)	
							)		
						
						)
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						dem=0
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view=axlUIViewFileCreate("via_name_chk.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("bb_via" `bb_via)
(defun bb_via ()
	sub_etch = nil
	foreach(sub axlSubclassRoute(?field 'isEtch)
		sub_etch = cons(strcat("ETCH/" sub) sub_etch )
	)
	sub_etch = reverse(sub_etch)
	axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view)
	)
	dem=0
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./bb_via.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-4s\n", "STT: ","NET :" "Location :")
			foreach(item setof(e Input_List index(e->name "BOTTOM") == nil)
					Item_Obj = item->objType
						if(Item_Obj == "via" then
							layer= item->startEnd
							;fprintf(Out_File, "asfsa %-5n %-15s %-15L\n",dem,car(item->startEnd), cadr(item->startEnd))
							;if(car(item->startEnd) != "ETCH/AL2" then
								;if(  cadr(item->startEnd) != "ETCH/BOTTOM" then
									st_via = car(item->startEnd)
									end_via = cadr(item->startEnd)
									chk1 = member(st_via sub_etch)
									chk2 = member(end_via reverse(chk1))
									if(length(chk2)>0 && length(chk2)<4 && index(item->name "TOP") == nil 
									&& item->prop->COMMENT == nil && setof(e axlDBGetConnect(item t) e->objType=="path") == nil then
										dem++
										axlHighlightObject(item)
										fprintf(Out_File, "%-5n %-15s %-15L %-20L\n",dem,item->name, item->startEnd, item->xy)
									)
									
								;)
							;)		
						)
					)
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view= axlUIViewFileCreate("bb_via.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("create_ps" `create_ps)
( defun create_ps ()
        name_current_design = axlCurrentDesign()
        if(isFileName("temp.brd") == t then  deleteFile("temp.brd"))
        axlOSFileCopy(strcat(getWorkingDir() "/" axlCurrentDesign() ".brd") strcat(getWorkingDir() "/temp.brd") nil)
        axlOpenDesign(?design strcat(getWorkingDir() "/" axlCurrentDesign() ".brd") ?mode "w" ?noMru t)
        axlClearSelSet()
        axlSetFindFilter(?enabled '(noall VIAS  NAMEFORM) ?onButtons '(noall VIAS))
        mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
        axlUIPopupSet(mypopup)
        vias = axlGetSelSet(axlSelect(?prompt "Please select a via..."))
        if(vias != nil then
            p_list = nil
            befor_ps_list = nil       
            foreach(ps axlDBGetDesign()->padstacks
                befor_ps_list = cons(ps->name befor_ps_list)
                
            )
            foreach(via vias p_list = cons(via->name p_list))
            STT =0
            Out_File = outfile("./create_ps.log" "w")
            p_list = unique(p_list)
            ps_name_list = nil
            fprintf(Out_File "%L\n" p_list)
            foreach(pad_stack_name p_list
                temp = axlLoadPadstack(pad_stack_name)
                st_via = cadr(parseString(car(temp->startEnd) "/") )
                end_via = cadr(parseString(cadr(temp->startEnd) "/") )
                layer_list = member(st_via axlSubclassRoute(?field 'isEtch))
                layer_list = reverse(member(end_via reverse(layer_list)))
                cut_layer_list = nil
                foreach(sub layer_list
                   foreach(sub2 cdr(member(sub layer_list))
                            cut_layer_list = cons(list(sub sub2)  cut_layer_list)
                   )
                )
                AL2 = nil
                    if(index( temp->name "AL1" ) != nil then
                            AL2 = "AL1"
                    else
                        if(index( temp->name "AL2" ) != nil then
                            AL2 = "AL2"
                        else
                            if(index(temp->name "AL" ) != nil then
                                AL2 = "AL"
                            )
                        )       
                    )
                    isetch= axlSubclassRoute(?field 'isEtch)
                    list1=  member(AL2 isetch)
                    list1=  member(AL2 isetch)
                    list2=  member("TOP" list1)
                        if(list2!= nil then
                        al_at = "BOTTOM"
                       else
                        al_at = "TOP"
                       )
                foreach(sub reverse(cut_layer_list)
                    name_via_temp = nil
                    if(member(AL2 sub) != nil then
                        if(al_at=="TOP" then
                            name_via_temp = strcat(car(parseString(temp->name "-")) "-" AL2 "-" cadr(sub) "-" lastelem(parseString(temp->name "-")))
                        else
                            name_via_temp = strcat(car(parseString(temp->name "-")) "-" car(sub) "-" AL2 "-" lastelem(parseString(temp->name "-")))
                        )
                    else
                        name_via_temp = strcat(car(parseString(temp->name "-")) "-" car(sub) "-" cadr(sub) )
                        
                    )
                    if( name_via_temp != nil 
                    && setof(e ps_name_list e == name_via_temp) == nil 
                    &&  member(name_via_temp ps_name_list) == nil 
                    && member(name_via_temp befor_ps_list) == nil 
                    && length( parseString(name_via_temp "-")) == length(unique(parseString(name_via_temp "-"))) then
                       newPadId = axlDBCopyPadstack(temp->name , list(strcat("ETCH/" car(sub) ) strcat("ETCH/" cadr(sub) )) nil)
                       axlPadstackToDisk(newPadId->name name_via_temp)
                       ps_name_list = cons(name_via_temp ps_name_list)
                    )  
                ) 
            
            )
              axlOpenDesign(?design name_current_design ?mode "w" ?noMru t)
              if(ps_name_list != nil then
               foreach(ps ps_name_list
                    axlCnsAddVia(nil ps)
                    name_file = strcat(ps ".pad")
                    if(isFileName(name_file) == t then
                        STT++
                        fprintf(Out_File, "%-5n %-10s\n" STT ps)
                        deleteFile(name_file)
                    )
              )
               close(Out_File)
              axlUIViewFileCreate("create_ps.log", "Show Element To Check Pins dummy" nil)
            )
       
         if(isFileName("temp.brd") == t then  deleteFile("temp.brd"))
         printf("%s\n" "finished")
    
    )
)
;########################################################################
axlCmdRegister("rename_via_chk" `rename_via_chk)
(defun rename_via_chk ()
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
	tt=0
	dem=0
	list_via_select = nil
	list_via_add = nil
	list_via_NC= nil
		if(length(Input_List) != 0 then
			Out_File = outfile("./rename_via_chk.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-30s %-4s\n", "STT: ","NET :" "VIA : " "Location :")
			foreach(item setof(e Input_List member("ETCH/BL" e->startEnd) == nil)
					Item_Obj = item->objType
						if(Item_Obj == "via" then
							cd1= car(item->startEnd);start layer
							cd2= cadr(item->startEnd);end layer
							st_via_bf = strcat("ETCH/" nth(1 parseString(item->name "-"))) ;first name
							end_via_bf = strcat("ETCH/" nth(2 parseString(item->name "-"))); end name
							if( cd1 != st_via_bf || cd2 != end_via_bf then
								dem++
                                if(member(item->name list_via_NC)== nil then
                                    list_via_NC = cons(item->name list_via_NC)
                                    fprintf(Out_File, "%-5n %-10L %-10L %-10L\n",dem,item->name,item->startEnd,item->xy)
                                )
							)
							
						)		
				)	
				fprintf(Out_File, "%s\n", "-------------------------------------------------")
				fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
				fprintf(Out_File, "%s\n", "-------------------------------------------------")
				close(Out_File)
				printf("Total: %n Pins Unconnect\n" dem)
						view=axlUIViewFileCreate("rename_via_chk.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("rename_via" `rename_via)
(defun rename_via ()
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
	dem=0
    db_via_list= nil
	if(length(Input_List) != 0 then
		Out_File = outfile("./rename_via.log" "w")
		foreach(item setof(e Input_List member("ETCH/BL" e->startEnd) == nil)
			Item_Obj = item->objType
			if(Item_Obj == "via" then
				parse_via_name = parseString(item->name "-")
				cd11= cadr(parseString(car(item->startEnd) "/"));start layer
				cd22= cadr(parseString(cadr(item->startEnd) "/"));end layer
				st_via_bf_1 = cadr(parse_via_name) ;first name
				end_via_bf_1 = caddr(parse_via_name); end name
				model_name_via = car(parse_via_name)
				if( cd11 != st_via_bf_1  || cd22 != end_via_bf_1 then
					dem++
					firstName = car(parse_via_name)
					if(length(parse_via_name)==3 then
						 db_via_list = cons( list(item strcat(firstName "-" cd11 "-" cd22)) db_via_list)
					else
						if(st_via_bf_1 != "BL" then
						  name_via = strcat(firstName "-" cd11 "-" index(item->name end_via_bf_1) )
						  db_via_list = cons( list(item name_via) db_via_list)
						  )
					) 
				) 			
			)		
		)
		foreach(item db_via_list
			dem++
			if( axlLoadPadstack(cadr(item)) != nil then
				chk1 = car(item)->prop->COMMENT
				if(chk1=="mycoment" then
					println(car(new_pad)->prop->COMMENT)
				else
					new_pad = axlReplacePadstack(car(item) cadr(item))
					axlDBAddProp(new_pad list( "Comment" "mycoment"))
					fprintf(Out_File, "%-10s %-10s %-10L\n", cadr(item)  car(item)->name car(item)->xy )
				)
			else
				fprintf(Out_File, "No via in Design%-10s %-10s %-10L\n", cadr(item)  car(item)->name car(item)->xy )
			)
		   
		)  	
		axlPurgePadstacks('padstacks nil)
		fprintf(Out_File, "%s\n", "-------------------------------------------------")
		fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
		fprintf(Out_File, "%s\n", "-------------------------------------------------")
		close(Out_File)
		printf("Total: %n Pins Unconnect\n" dem)
		view = axlUIViewFileCreate("rename_via.log", "Show Element To Check Pins dummy" nil)
		else
			printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("conn_shape_al2" `conn_shape_al2)
(defun conn_shape_al2 ()
	;yesno=axlUIYesNo( " Would you like to add pick ?" "Skill Show No connect Pin" )
		axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	Input_List = nil
	get_db_all=list()
	count_cline=0
	count_net=0
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view))
	dem=0
	count_cline=0
	count_net=0
	Input_List = axlGetSelSet()
	axlClearSelSet()
	get_db_all=nil
		if(length(Input_List) != 0 then
			Out_File = outfile("./conn_shape_al2.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-4s\n", "STT: ","NET :" "Location :")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" && length(axlDBGetConnect(item))!=1 then
						;printf("%L\n" length(axlDBGetConnect(item)))
									; begin
									net_name=item->net->name					
									layer_shape="etch/al2"
									;printf("box 111 : %L\n" layer_pin)
									poly = car(axlPolyFromDB(item, ?endCapType 'SQUARE ?layer layer_shape ?padType 'REGULAR))
									expand_value= fix(axlDistance(car(item->bBox) cadr(item->bBox))/2)
									Item_poly_Expand = car(axlPolyExpand(poly expand_value 'NONE)) 
									axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS ))
									get_db_all=axlGetSelSet(axlAddSelectBox(Item_poly_Expand->bBox))	
									axlClearSelSet()
									foreach(get_db get_db_all
												if(net_name == get_db->net->name  then
													if( nth(2 axlAirGap( get_db item)) !=nil && nth(2 axlAirGap( get_db item)) >0 then 
														count_net++	
													)
													
												)
									
											)
									if(count_net++ > 1  then 
											foreach(conn_st axlDBGetConnect(item t)
													if(conn_st->objType =="shape" && conn_st->layer == "ETCH/AL2" then
													count_cline++
													
														)
													)
											if(count_cline == 0 then
													dem++
													fprintf(Out_File, "%-5n %-15s %-20L\n",dem,net_name,item->xy)	
													axlHighlightObject(item)	
											)
										)
										count_cline=0
										count_net=0
									
									
								;end
							)		
						
						)
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						dem=0
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view=axlUIViewFileCreate("conn_shape_al2.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("conn_cline_via" `conn_cline_via)
(defun conn_cline_via ()

	yesno=axlUIYesNo( " Would you like to add pick ?" "Skill Show No connect Pin" )
	input = parseString(axlEnterString(
            ?prompts list("Please enter a layer :")))
	layer_in=strcat("ETCH/" upperCase(car(input)))
printf("The layer entered : %s\n " layer_in)	
		axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view)
	)
	dem=0
	Input_List = axlGetSelSet()
	axlClearSelSet()
	get_db_all=nil
		if(length(Input_List) != 0 then
			Out_File = outfile("./conn_cline_via.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-4s\n", "STT: ","NET :" "Location :")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" then
										net_name=item->net->name		
									foreach(con_st axlDBGetConnect(item t)
										
										if(con_st->objType =="path" && con_st->layer==layer_in then 
										
												dem++
												if(yesno == nil then 
											fprintf(Out_File, "%-5n %-15s %-20L\n",dem,item->net->name,item->xy)
											else
											fprintf(Out_File, "pick %L\n",item->xy)
											)
											axlHighlightObject(item)	
											)
									)	
									
								
							)		
						
						)
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view=axlUIViewFileCreate("conn_cline_via.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("re_via" `re_via)
(defun re_via ()
Input_List=""
defun( next_demo ()
axlClearDynamics()
axlFinishEnterFun()
Input_List=""
input=nil
tam=nil
printf("Total: %L test next\n" "next")
)
while(Input_List=="" 
		input = parseString(axlEnterString(
            ?prompts list("Please enter a layer :")))
			axlSetFindFilter(?enabled "vias" ?onButtons "vias")
						
						tam=car(axlGetSelSet(axlSelect(?prompt "Please select a via...")))
						via_st=tam->name
						layer_end=cadr(input)
						printf("Via name select : %s\n", via_st)
						printf("Via name select : %s\n", layer_end)
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Next" 'next_demo)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	dem=0
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./re_via.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-30s %-4s\n", "STT: ","NET :" "VIA : " "Location :")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" && axlDBIsFixed(item)==nil then
								
								if(car(item->startEnd)==strcat("ETCH/" upperCase(car(input))) && cadr(item->startEnd)== strcat("ETCH/" upperCase(cadr(input))) then
									dem=dem +1
									
									fprintf(Out_File, "%-5n %-15s %-25s %-25s %-10L\n",dem,item->net->name,item->name,via_st,item->xy)	
									kq=axlReplacePadstack(item via_st)
									
								)	
							)		
						
						)
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						dem=0
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view=axlUIViewFileCreate("re_via.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
)
;########################################################################
axlCmdRegister("sense_diff_via" `sense_diff_via)
(defun sense_diff_via ()
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	dem=0
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./sense_diff_via.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-30s %-4s\n", "STT: ","NET :" "VIA : " "Location :")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" then
								axlSetFindFilter(?enabled '(noall PINS  NAMEFORM) ?onButtons '(noall PINS))
								pin=axlGetSelSet(axlSingleSelectPoint(item->xy))
								
									dem=dem +1
									fprintf(Out_File, "%-5n %-25s %-25s %-10L\n",dem,item->net->name,car(pin)->net->name,item->xy)	
									
							)		
						
						)
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						dem=0
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view=axlUIViewFileCreate("sense_diff_via.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)

;########################################################################
axlCmdRegister("pad_chk" `pad_chk)
(defun pad_chk ()
	
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	dem=0
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./pad_chk.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-30s %-4s\n", "STT: ","NET :" "VIA : " "Location :")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" then
								
									dem=dem +1
									
									fprintf(Out_File, "%-5n %-40s %-30L %-4L\n",dem,item->net->name,item->definition->?? item->xy)
									
								
							)		
						
						)
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						dem=0
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view=axlUIViewFileCreate("pad_chk.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("conn_via_cline" `conn_via_cline)
(defun conn_via_cline ()	
					
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES SHAPES NAMEFORM) ?onButtons '(noall CLINES))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List =nil
	list_st =nil
	start=end=nil
	layers=nil
	SubClasses=nil
	
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view)
	list_st =nil
	layers=nil
	SubClasses=nil
	)
	Input_List = axlGetSelSet()
	axlClearSelSet()
	Out_File = outfile("./conn_via_cline.log" "w")
	
			if(length(Input_List) != 0 then	
				foreach(item Input_List
					Item_Obj = item->objType
							if( Item_Obj == "path" then
									if(axlDBIsFixed(item) ==nil then
									foreach(con_st axlDBGetConnect(item t)
												
												if( con_st->name !=nil then
														
												;fprintf(Out_File, "%-20L %-15L\n",con_st->name, item->layer)
												foreach(temp_st axlDBGetConnect(con_st t)
												;fprintf(Out_File, "%-20L %-15L\n",con_st->name, car(temp_st->segments)->layer)
														type=temp_st->objType
														
													case( type
															("pin" 
															;fprintf(Out_File, "pin %-20L %-15L\n",con_st->name,temp_st->startEnd)
															layers=cons(list( con_st temp_st->startEnd ) layers)
															)
															("via"
																
																foreach(temp_1 axlDBGetConnect(temp_st t)
																	if( axlDBGetConnect(temp_st t)	=="shape" then
															fprintf(Out_File, "%-30s %-15s %-15L\n",temp_1->net->name,con_st->name,con_st->xy)
																	)
															)
															
															)
															("shape"
															;fprintf(Out_File, "shape %-20L %-15L\n",con_st->name, car(temp_st->segments)->layer)
															layers=cons(list(con_st car(temp_st->segments)->layer) layers)
															)
															("path"
															;fprintf(Out_File, " path %-20L %-15L\n",con_st->name, car(temp_st->segments)->layer)
															layers=cons(list(con_st car(temp_st->segments)->layer) layers)
															)
													
													
													)
													)
													
														)
													
												)
												
												
										)
										
											
							)
						)
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
							dem=0
				close(Out_File)
			
				printf("Total: %n Pins Unconnect\n" dem)
				view=axlUIViewFileCreate("conn_via_cline.log", "Show Element To Check Pins" nil)
			else
				printf("No Element found!\n")
				
			)
	)	
)
;########################################################################
axlCmdRegister("count_vi" `count_vi)
(defun count_vi ()
Input_List=""
defun( next_demo ()
axlClearDynamics()
axlFinishEnterFun()
Input_List=""
axlUIWClose(view)
printf("Total: %L test next\n" "next")
)
while( Input_List=="" 					
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)(list "Next" 'next_demo)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	input=nil
	list_via = nil
	while(axlSelect(?prompt "Please select an element...")			
	if( Input_List != nil then
	dem=0
	input=nil
	list_via = nil
	axlUIWClose(view)
	)
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./count_vi.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-30s %-4s\n", "STT: ","NET :" "VIA : " "Location :")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" then
								if(item->net->name != nil ||item->net->name !=""  then
									if(axlDBIsFixed(item) == nil then
											axlHighlightObject(item)
											list_via= cons(item list_via)
									)
								)	
						)		
						
				)
				
			
					axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall NETS ))
					input= nil
					tam=car(axlGetSelSet(axlSelect(?prompt "Please select a via...")))
					input=tam->net->name
					printf("Net name select : %L\n", input)
					axlClearSelSet()
						if( input != nil then
								foreach( via_st list_via
										if( via_st->net->name == input then
											dem++
											name1= via_st->name
												;fprintf(Out_File, "%-5n %-20s %-15s %-20L\n" dem,input,via_st->name,via_st->xy)
											axlDehighlightObject(via_st)	
											)
										)
										fprintf(Out_File, "%-5n %-15s %-20L\n" dem,input,name1)
										
							)
				
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Vias\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						dem=0
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view=axlUIViewFileCreate("count_vi.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
	)
 )
;########################################################################
axlCmdRegister("ass_via1" `ass_via1)
(defun ass_via1 ()
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view)
	)
	Input_List = axlGetSelSet()
	axlClearSelSet()
	dem=0
		if(length(Input_List) != 0 then
			;Out_File = outfile("./ass_via1.log" "w")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" then
							axlClearSelSet()	
							axlSetFindFilter(?enabled '(noall SHAPES NAMEFORM) ?onButtons '(noall SHAPES ))
								shape = axlGetSelSet(axlAddSelectBox(item->bBox))
								
								;fprintf(Out_File, "%-5n %-10L %-10L\n",dem,"AGND",car(pin)->xy)
								if( length(member(car(shape)->layer item->startEnd)) > 0 then
									axlClearSelSet()
									axlSetFindFilter(?enabled '(noall PINS NAMEFORM) ?onButtons '(noall PINS ))
									pin= axlGetSelSet(axlAddSelectBox(item->bBox))
									
									dem++
									
									axlDBAssignNet(pin car(shape)->net->name t)
									
								)
								shape = nil
								pin = nil
								axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
							)		
						
						)
						
						;fprintf(Out_File, "%s\n", "-------------------------------------------------")
						;fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						;fprintf(Out_File, "%s\n", "-------------------------------------------------")
						;close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						;view=axlUIViewFileCreate("ass_via1.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("ass_via" `ass_via)
(defun ass_via ()
	;axlSetFindFilter(?enabled '(noall NETS  NAMEFORM) ?onButtons '(noall NETS))
	;input_via=car(axlGetSelSet(axlSelect(?prompt "Please select a net...")))
	;via_tem=parseString(input_via->net->name "_")
	;printf("net name is cutting : %L\n" car(via_tem))
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view)
	)
	Input_List = axlGetSelSet()
	axlClearSelSet()
	dem=0
		if(length(Input_List) != 0 then
			Out_File = outfile("./ass_via.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-30s %-4s\n", "STT: ","NET :" "VIA : " "Location :")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" then
									axlSetFindFilter(?enabled '(noall PINS NAMEFORM) ?onButtons '(noall PINS ))
									pin=axlGetSelSet(axlSingleSelectPoint(item->xy))
									dem++
									axlDBAssignNet(pin item->net->name t)
									
								;if( cadr(parseString(item->net->name "_")) !=nil then
									;dem++
									;net_conver=strcat(car(via_tem) "_" cadr(parseString(item->net->name "_")))
									;printf("net name assign to pin: %L\n" net_conver)
									;axlDBAssignNet(pin net_conver t )
									;fprintf(Out_File, "%-5n %-10s %-10L\n",dem,net_conver,car(pin)->xy)
								;)
								;if( item->net->name =="DGND" then
									;dem++
									
								;	printf("net name assign to pin : %s\n" "DGND")
									;axlDBAssignNet(pin "DGND" t )
									;fprintf(Out_File, "%-5n %-10s %-10L\n",dem,"DGND",car(pin)->xy)
								;)
								;if( item->net->name =="AGND" then
								;	dem++
									
								;	printf("net name assign to pin : %s\n" "AGND")
								;	axlDBAssignNet(pin "AGND" t )
								;	fprintf(Out_File, "%-5n %-10s %-10L\n",dem,"AGND",car(pin)->xy)
								;)
								
							)		
						
						)
						axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view=axlUIViewFileCreate("ass_via.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("vianet2" `vianet2)
vianet2_ip = nil
(defun vianet2_custom ()
	vianet2_ip_ = axlEnterString(?prompts list("Please enter params :"))
	 if(vianet2_ip_ != nil then
		vianet2_ip = vianet2_ip_
	 )

)
(defun vianet2 ()
	sub_info = car(setof(e axlVisibleGet() member("ETCH" e) != nil ))->subclassinfo
	sub_vis = caar(setof(e sub_info cadr(e) == t))
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun) (list "Custom" 'vianet2_custom)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	dem=0
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./via_net2.log" "w")
			foreach(via Input_List
				cus_list = list(via->net->name via->name via->prop->COMMENT)
				cus_list = setof(e cus_list e != nil)
				if(vianet2_ip != nil then
					vianet2_ip = upperCase( vianet2_ip)
					chk = nil
					if(index(vianet2_ip " ")!= nil && index(vianet2_ip "*")== nil then
						chk_tmp = setof(e parseString(vianet2_ip " ") e !="PICK")
						chk2= nil
						foreach(item chk_tmp
							chk_ = setof(e cus_list e == item)
							if(chk_ != nil then
								chk2 = cons(t chk2)
							)
						)
						;printf("%L %L" length(chk2) length(chk_tmp))
						if(length(chk2) == length(chk_tmp) then
							chk = t
						)
					else
						if(index(vianet2_ip "*")!= nil then
							chk = setof(e cus_list index(e  car(parseString(vianet2_ip "*")))!= nil)
						else
							chk = setof(e cus_list e==  vianet2_ip)
						)
						
					)
					if(chk != nil then
						if(index(vianet2_ip "PICK") != nil then
							fprintf(Out_File, "pick %L\n", via->xy)
						else
							dem++
							fprintf(Out_File, "%-5d %-20s %L\n", dem buildString(cus_list " ") via->xy)
						)
						
					)
					
				else
					dem++
					fprintf(Out_File, "%-5d %-20s %L\n", dem buildString(cus_list " ") via->xy)
				)
						
			)
			
			fprintf(Out_File, "%s\n", "-------------------------------------------------")
			fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
			fprintf(Out_File, "%s\n", "-------------------------------------------------")
			dem=0
			close(Out_File)
			printf("Total: %n Pins Unconnect\n" dem)
			view= axlUIViewFileCreate("via_net2.log", "Show Element To Check Pins dummy" nil)
		else
			printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("vianet" `vianet)
(defun vianet ()
	sub_info = car(setof(e axlVisibleGet() member("ETCH" e) != nil ))->subclassinfo
	sub_vis = caar(setof(e sub_info cadr(e) == t))
		
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	dem=0
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./via_net.log" "w")
			;fprintf(Out_File, "%-5s %-40s %-30s %-4s\n", "STT: ","NET :" "VIA : " "Location :")
			foreach(via Input_List
					Item_Obj = via->objType
						if(Item_Obj == "via" then
							;pwr_list = list("VSS" "PEO_IO_VDD" "PEO_VDD" "HBM_DOWN_DC_S" "MAINCHAIN_TAP_N_S" "RSVD" "HBM_UP_DC_S" "SPARE")
							;if(member(via->net->name pwr_list) != nil && via->name=="VIA50-TOP-MP2" then
								fprintf(Out_File, "%-15s %-15s %-15L\n" , via->net->name  via->name via->xy)	
							;)
								;if(item->net->name !="GND" then
									dem=dem +1
									
									;fprintf(Out_File, "%-5n %-10L%-15L %-15L %-15L\n",dem, sub_vis , sprintf(nil "%L" via->net->name),via->name via->xy)	
									
									
									; if(item->prop->COMMENT != nil then 
										; via_comment = item->prop->COMMENT
										; if(index(via_comment item->net->name ) == nil then
										; dem=dem +1
										; fprintf(Out_File, "%-5n %-15L %-15L %-15L\n",dem,item->net->name,item->name item->xy)	
										; )
									; )
								;)
								; if(index(via->name "T-") then
									; cus_name_via = sprintf(nil "%s-%s-%s" car(parseString(via->name "T" )) cadr(parseString(via->name "-" )) caddr(parseString(via->name "-" )))
									; fprintf(Out_File, "%-5n %-15L %-15L %-15L\n",dem,via->net->name,cus_name_via via->xy)	
									; axlReplacePadstack(via cus_name_via )
								; else
									; ; cus_name_via = sprintf(nil "%sT-%s-%s" car(parseString(via->name "-" )) cadr(parseString(via->name "-" )) caddr(parseString(via->name "-" )))
									; ; fprintf(Out_File, "%-5n %-15L %-15L %-15L\n",dem,via->net->name,cus_name_via via->xy)	
									; ; axlReplacePadstack(via cus_name_via )
								; )
							)		
						
						)
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						dem=0
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view= axlUIViewFileCreate("via_net.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("conn_via" `conn_via)
(defun conn_via ()
	input_file = axlDMFileBrowse(nil nil ?title "Please choose file create Pins ..." ?optFilters "All text files|*.txt|")
	
	
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	dem=0
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./conn_via.log" "w")
			foreach(item Input_List
					Item_Obj = item->objType
						if(Item_Obj == "via" then
									inPort = infile(input_file)		
								when(inPort
									while(gets(nextline inPort)
											
										if(length(parseString(nextline)) >=1 then
											nextline1 = car(parseString(nextline, "\n"))
											nextline1 = parseString(nextline1)
											request=car(nextline1)
											net_st=nth(1 nextline1)
											if(net_st == item->net->name  then
												if(length(axlDBGetConnect(item t)) != atof(request)+1   then	
														conn_line = setof(x axlDBGetConnect(item)(x->objType =="path" || x->objType =="line"))
													;if(item->net->name != nil && length(conn_line) ==0 then
													if(item->net->name != nil then
														dem=dem +1
															fprintf(Out_File, "%-5n %-40s %-30s %-4L\n",dem,item->net->name,item->name item->xy)
																axlHighlightObject(item)
																
														)
													)
											)
											else
														printf("Stoped! Invalid database.\n")
											);end if 
								
										);end while
								);end when		
						
							)		
						
						)
						
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						dem=0
						close(Out_File)
						printf("Total: %n Pins Unconnect\n" dem)
						view= axlUIViewFileCreate("conn_via.log", "Show Element To Check Pins dummy" nil)
			else
				printf("No Element found!\n")
					
		)
	)
 
)
;########################################################################
axlCmdRegister("via_area" `via_area)
(defun via_area ()
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS GROUPS CLINES LINES SHAPES NAMEFORM) ?onButtons '(noall VIAS ))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	dem1=0
	temp=""
	Input_List = nil
	list_item=nil
	list_temp=nil
	SubClasses=nil
	p=axlGetParam("paramDesign")->accuracy
				tol=expt(10 -p)*0.00009
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view)
	Input_List = nil
	list_item=nil
	list_temp=nil
	dem=0
	dem1=0
	)
	Input_List = axlGetSelSet()
	axlClearSelSet()
			if(length(Input_List) != 0 then
				Out_File = outfile("./via_area.log" "w")
				;fprintf(Out_File,"%-5s %-15s\n","STT" ,"Layer")
				;fprintf(Out_File,"%s\n","---------------------------------------------------------------------------------------------")
				foreach(item Input_List	
						Item_Obj = item->objType
						if(Item_Obj =="via" && item->net != nil then
								
										fprintf(Out_File,"%-15.4f %-15.4f %-25s\n",xCoord(item->xy) yCoord(item->xy),item->name)					
								
							)
							
									
						)				
				close(Out_File)
				printf("Total: %n Cline checked by in skill program\n" dem)
				;view=axlUIViewFileCreate("via_area.log", "Show Element To Check Cline" nil)
				axlOpenDesign(?design nil ?mode "wf" ?noMru nil)
				axlSetFindFilter(?enabled "vias" ?onButtons "vias")
				shape_all=axlGetSelSet(axlSelect(?prompt "Please select a via..."))
				foreach( data_via shape_all
					
					list_item= reverse(cons(list(data_via->xy data_via->name) list_item))
					;printf("%L\n" nth(1 parseString(data_via->layer "./")))
					
				)
				inPort = infile("via_area.log")
				Out_File1 = outfile("./via_area1.log" "w")
				when(inPort
				
					while(gets(nextline inPort)
							
						if(length(parseString(nextline)) >=1 then
						nextline1 = car(parseString(nextline, "\n"))
						;printf("%L\n" nextline1)
						nextline1 = parseString(nextline1)
						xy1=atof(nth(0 nextline1)):atof(nth(1 nextline1) )
						layer_infile=nth(2 nextline1)
						printf("%L\n" layer_infile)
					
						foreach( find_item list_item
										
									if(  axlDistance(nth(0 find_item) xy1) ==0  then
											temp=cadr(find_item)
										if(cadr(find_item) == layer_infile then
										dem++;
										
										
									;fprintf(Out_File1,"%5n %-20L %-15s\n" dem axlDistance(nth(0 find_item) xy1) cadr(find_item))
									)
									
									)	
								)
								if(dem == 0 then
									
								dem1++;
								fprintf(Out_File1,"%5n %-20L %-15s %-15s\n" dem1 xy1 layer_infile temp)
								)
								temp=""
								dem=0
						
						
						
					
					
							else
									printf("Stoped! Invalid database.\n")
							);end if 
				
					);end while
				);end when
				if(abs(length(list_item) - dem1) != 0 then
				fprintf(Out_File1," Error : %n\n", abs(length(list_item) - dem1) )
				)
				close(Out_File1)
				view1=axlUIViewFileCreate("via_area1.log", "Show Element To Check Cline" nil)
		printf("Finish ! Creation's %n errors\n", abs(length(list_item) - dem1) )
		close(inPort)
		
				
			else
				printf("No Element found!\n")
				
			)
	)	
)
;########################################################################
axlCmdRegister("via_anten" `via_anten)
(defun via_anten ()	
					axlSetFindFilter(?enabled "vias" ?onButtons "vias")
						input=nil
						while( input ==nil ||input ==""
						tam=car(axlGetSelSet(axlSelect(?prompt "Please select a via...")))
						input=tam->name
						axlClearSelSet()
						)
						printf("Via name select : %s\n", input)
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES SHAPES NAMEFORM) ?onButtons '(noall CLINES))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List =nil
	list_st =nil
	start=end=nil
	layers=nil
	SubClasses=nil
	
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view)
	list_st =nil
	layers=nil
	SubClasses=nil
	)
	Input_List = axlGetSelSet()
	axlClearSelSet()
	Out_File = outfile("./via_anten.log" "w")
	
			if(length(Input_List) != 0 then	
				foreach(item Input_List
					Item_Obj = item->objType
							if( Item_Obj == "path" then
									if(axlDBIsFixed(item) ==nil then
									foreach(con_st axlDBGetConnect(item t)
												
												if( con_st->name !=nil then
														
												;fprintf(Out_File, "%-20L %-15L\n",con_st->name, item->layer)
												foreach(temp_st axlDBGetConnect(con_st t)
												;fprintf(Out_File, "%-20L %-15L\n",con_st->name, car(temp_st->segments)->layer)
														type=temp_st->objType
														
													case( type
															("pin" 
															;fprintf(Out_File, "pin %-20L %-15L\n",con_st->name,temp_st->startEnd)
															layers=cons(list( con_st temp_st->startEnd ) layers)
															)
															("via"
																
															;fprintf(Out_File, " via %-20L %-15L\n",con_st->name,temp_st->startEnd)
															layers=cons(list( con_st temp_st->startEnd) layers)
															)
															("shape"
															;fprintf(Out_File, "shape %-20L %-15L\n",con_st->name, car(temp_st->segments)->layer)
															layers=cons(list(con_st car(temp_st->segments)->layer) layers)
															)
															("path"
															;fprintf(Out_File, " path %-20L %-15L\n",con_st->name, car(temp_st->segments)->layer)
															layers=cons(list(con_st car(temp_st->segments)->layer) layers)
															)
													
													
													)
													
														)
													
												)
												
												
										)
										
												;show layers
													foreach(ly_st layers
													;if(car(ly_st)->name != input &&axlDBIsFixed(car(ly_st))== nil then
													if(car(ly_st)->name !=nil then
													
													
													dem++;
													;fprintf(Out_File, " %-5n %-25s %-15s %-15L\n",dem, car(ly_st)->net->name,car(ly_st)->name car(ly_st)->xy)
													fprintf(Out_File, "pick %-15L\n", car(ly_st)->xy)
													axlDBAddProp(axlGetSelSet(car(ly_st)) list( "Comment" "sense_via_delete"))
													;kq=axlReplacePadstack(car(ly_st) input)
													)		
													)
													layers=nil
													;end show
								
								else
								printf("Via's name : %s Fixed \n" item->name)
								)	
							)
						)					
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
							dem=0
				close(Out_File)
			
				printf("Total: %n Pins Unconnect\n" dem)
				view=axlUIViewFileCreate("via_anten.log", "Show Element To Check Pins" nil)
			else
				printf("No Element found!\n")
				
			)
	)	
)
;########################################################################
axlCmdRegister("export_via" `export_via)
(defun export_via ()
dir= getWorkingDir()
dir_new= strcat( dir "/Dumb")
if(isDir(dir_new) != t then
    createDir(dir_new)
)

yesno=axlUIYesNo( " Would you like to export all vias ? " "Skill EXPORT VIA" )
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES SHAPES NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	Input_List =nil
	
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view))
	Input_List = axlGetSelSet()
	axlClearSelSet()
	Out_File = outfile("./export_via.log" "w")
			if(length(Input_List) != 0 then	
				foreach(item Input_List
					Item_Obj = item->objType
						if( Item_Obj == "via" then
							
								if(yesno == t then
								axlPadstackToDisk()		
								 else
								 dem= dem+1
								axlPadstackToDisk(item->name)
									
								)
							)
						)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
						fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
						fprintf(Out_File, "%s\n", "-------------------------------------------------")
							dem=0
				close(Out_File)
			
				printf("Total: %n Pins Unconnect\n" dem)
				view=axlUIViewFileCreate("export_via.log", "Show Element To Check Pins" nil)
			else
				printf("No Element found!\n")
				
			)
	)	
)

;########################################################################
axlCmdRegister("change_via_v1" `change_via_v1)
(defun change_via_v1 ()
	input = axlEnterString(?prompts list("Please enter your size via:"))
	axlClearSelSet()
	axlSetFindFilter(?enabled '(noall VIAS NAMEFORM) ?onButtons '(noall VIAS))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	dem=0
	view = nil
	Input_List = nil
	while(axlSelect(?prompt "Please select an element...")
		if( Input_List != nil then
		dem=0
		axlUIWClose(view))
		Input_List = axlGetSelSet()
		axlClearSelSet()
		if(length(Input_List) != 0 then
			Out_File = outfile("./change_via_v1.log" "w")
			foreach(item setof(e Input_List axlDBIsFixed(e->net) == nil)
					input_p = parseString(input " ")
					if(length(input_p) ==2 then
						if(axlDBIsFixed(item) == nil then
							dem=dem +1
							new_via_name = cadr(input_p)
							if( item->name == car(input_p) then 
								if(axlLoadPadstack(new_via_name) != nil  then 
									fprintf(Out_File,"%-5n %-20s %-35s %-35L\n" dem item->name  new_via_name item->xy )
									new = axlReplacePadstack(item new_via_name)
									axlHighlightObject(new)	
								)
							)
						)	
					)
						
				)	
			fprintf(Out_File, "%s\n", "-------------------------------------------------")
			fprintf(Out_File, "Issues exists in your design : %n Pins\n", dem)
			fprintf(Out_File, "%s\n", "-------------------------------------------------")
			dem=0
			close(Out_File)
			printf("Total: %n Pins Unconnect\n" dem)
			view= axlUIViewFileCreate("change_via_v1.log", "Show Element To Check Pins dummy" nil)
		else
			printf("No Element found!\n")
					
		)
	)
 
)
;########################################################
axlCmdRegister("change_via" `change_via ?cmdType "interactive")
cvia_input = nil
(defun change_via () 
	let( ( (input nil) ) 
		if(cvia_input == nil then
			axlClearSelSet()
			axlSetFindFilter(?enabled "vias" ?onButtons "vias")
			axlSelect(?prompt "Please select a via...") 
			input= car(axlGetSelSet())->name
			;axlShellPost("change_via")
			axlFinishEnterFun()
			axlCancelEnterFun()
		else
			input = axlUIPrompt("Enter a Via Name :" cvia_input )	
			f1 = nil
			if(input == nil && f1 == nil then
				f1 = t
				axlClearSelSet()
				axlSetFindFilter(?enabled "vias" ?onButtons "vias")
				axlSelect(?prompt "Please select a via...") 
				input= car(axlGetSelSet())->name
				;axlShellPost("change_via")
				axlFinishEnterFun()
				axlCancelEnterFun()
				
			
			)
		)
		if(input != nil then cvia_input =input
		printf("a Via name selected : %s\n", input)				
		axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS CLINES SHAPES NAMEFORM) ?onButtons '(noall VIAS))
		mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)(list "Next" 'change_via)))
		axlUIPopupSet( mypopup)
			while(axlSelect(?prompt "Please select an element...")
				dem=0
				Input_List = nil
				Input_List = axlGetSelSet()
				axlClearSelSet()
				if(length(Input_List) != 0 then	
					foreach(item setof(e Input_List (e->name != input) )
							if(axlDBIsFixed(item) == nil then
								axlReplacePadstack(item input)
								dem=dem+1	
								axlShell("redraw")
							;else
								;printf("Via's name : %s Fixed \n" item->name )
							)	
					)	
					printf("Total: %n Successful Replacing!\n" dem)
				else
					printf("No Element found!\n")
					
				)
			)
		)
	)	
)
;########################################################
axlCmdRegister("show_radius_via" `show_radius_via)
(defun show_radius_via ()
input=nil
yesno=nil
	yesno = axlUIYesNo( " Would you like to check size via ? -> press 'No' to show vias to list <- " "Skill Show Radius VIA" )
	if( yesno == t then 
		input=parseString(axlEnterString(?prompts list("Please enter your number of radius:")))
		)
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS GROUPS CLINES LINES SHAPES NAMEFORM) ?onButtons '(noall VIAS ))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	select_visible=nil
	dem=0
	solan=0;
	Input_List = nil
	len_list=nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view)
	len_list=nil
	Input_List = nil
	select_visible=nil
	dem=0)
	Input_List = axlGetSelSet()
	Input_List = axlGetSelSet()
	axlClearSelSet()
			if(length(Input_List) != 0 then
				Out_File = outfile("./show_radius_via.log" "w")
				fprintf(Out_File,"%-5s %-35s %-15s %-15s %-25s\n\n","STT","Name :","Radius :" ,"Layer :","Location :")
				;fprintf(Out_File,"%s\n","---------------------------------------------------------------------------------------------")
				foreach(item Input_List	
						Item_Obj = item->objType
						if(Item_Obj =="via" && item->net != nil && item->net->name !="" then ; 
								mid_box=axlDBAltOrigin('center item)
								select_visible=axlVisibleGet()
								foreach(vis select_visible
								if(nth(6 vis) != nil then
											foreach(vis1 nth(6 vis) 	
													if( nth(2 vis) == "VIA CLASS" && car(cdr(vis1)) ==t 
													&& car(axlPolyFromDB(item, ?layer strcat("ETCH/" car(vis1)) ?padType 'REGULAR))->bBox != nil   then	
													
													data1=car(axlPolyFromDB(item, ?layer strcat("ETCH/" car(vis1)) ?padType 'REGULAR))->bBox
													;printf("out1 %-10L out2 %-10L\n" nth(2 vis),data1)	
													h=axlDistance(car(data1) car(cdr(data1)))
													a=abs(h)/sqrt(2)
													if(car(input) != nil then 
													tol1=atof(car(input))
													tol2=abs(abs(a) - abs(tol1))
															if(yesno ==t && a != tol1 && tol2 > 0.0009  then
																dem=dem+1
																fprintf(Out_File,"%-5n %-35s %-15.3f %-15s %-25L\n",dem,item->name,a, car(vis1),mid_box)	
															)
														)
													if(input==nil && yesno==nil && a !=nil then
													dem=dem+1
													fprintf(Out_File,"%-5n %-35s %-15.3f %-15s %-25L\n",dem,item->name,a, car(vis1),mid_box)
													)	
												)		
											)
										)
									)	
							)					
						)
						
				close(Out_File)
				printf("Total: %n Cline checked by in skill program\n" dem)
				
				view=axlUIViewFileCreate("show_radius_via.log", "Show Element To Check Cline" nil)
			else
				printf("No Element found!\n")
				
			)
	)	

)
;########################################################
axlCmdRegister("via_al2_chk" `via_al2_chk)
(defun via_al2_chk ()
	input=nil
		input= parseString(axlEnterString(?prompts list("Please enter your size via:")))
		
	axlSetFindFilter(?enabled '(noall SYMBOLS PINS VIAS NETS GROUPS CLINES LINES SHAPES NAMEFORM) ?onButtons '(noall VIAS ))
	mypopup = axlUIPopupDefine(nil (list (list "Done" 'axlFinishEnterFun) (list "Cancel" 'axlCancelEnterFun)))
	axlUIPopupSet( mypopup)
	select_visible=nil
	dem=0
	solan=0;
	Input_List = nil
	len_list=nil
	while(axlSelect(?prompt "Please select an element...")
	if( Input_List != nil then
	axlUIWClose(view)
	len_list=nil
	Input_List = nil
	select_visible=nil
	dem=0)
	Input_List = axlGetSelSet()
	Input_List = axlGetSelSet()
	axlClearSelSet()
			if(length(Input_List) != 0 then
				Out_File = outfile("./via_al2_chk.log" "w")
				fprintf(Out_File,"%-5s %-35s %-15s %-15s %-25s\n\n","STT","Name :","Radius :" ,"Layer :","Location :")
				;fprintf(Out_File,"%s\n","---------------------------------------------------------------------------------------------")
				foreach(item Input_List	
						Item_Obj = item->objType
						if(Item_Obj =="via" && item->net != nil && item->net->name !="" then ; 
								mid_box=axlDBAltOrigin('center item)
								select_visible=axlVisibleGet()
								foreach(vis select_visible
								if(nth(6 vis) != nil then
											foreach(vis1 nth(6 vis) 	
													if( nth(2 vis) == "VIA CLASS" && car(cdr(vis1)) ==t 
													&& car(axlPolyFromDB(item, ?layer strcat("ETCH/" car(vis1)) ?padType 'REGULAR))->bBox != nil   then	
													
													data1=car(axlPolyFromDB(item, ?layer strcat("ETCH/" car(vis1)) ?padType 'REGULAR))->bBox
													;printf("out1 %-10L out2 %-10L\n" nth(2 vis),data1)	
													h=axlDistance(car(data1) car(cdr(data1)))
													a=abs(h)/sqrt(2)
													if(car(input) != nil then 
													tol1=atof(car(input))
													tol2=abs(abs(a) - abs(tol1))
															if(a < tol1 && tol2 > 0.0009  then
																dem=dem+1
																fprintf(Out_File,"%-5n %-35s %-15.3f %-15s %-25L\n",dem,item->name,a, car(vis1),mid_box)	
															)
														)
														
												)		
											)
										)
									)	
							)					
						)
						
				close(Out_File)
				printf("Total: %n Cline checked by in skill program\n" dem)
				
				view=axlUIViewFileCreate("via_al2_chk.log", "Show Element To Check Cline" nil)
			else
				printf("No Element found!\n")
				
			)
	)	

)
;########################################################
