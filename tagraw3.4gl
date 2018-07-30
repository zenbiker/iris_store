# @(#)tagraw3.4gl	1.1 - 12/17/09 - John Prochaska - 01/09/09

-- Program: tagraw3.4gl
-- Author : Gordon Wong
-- Date:    6/17/96
-- Description: Queries store host tables, Innovax item table, and  batch table
--              to create ASCII output file for Sign and Label program.
--
--  ad_type for Raleys are:  1=ad, 5=Bonus Buy, 9=Value Planner
--  

-- Changes:
--   2/18/97 - ggw - Added fields and calculations for ad prices and dates.
--   4/16/97 - ggw - Added fields for desc_sign2 and tag_quan
--   4/28/97 - ggw - Could not force regular tags if item was set up with
--                   a "T" in TPR field or if ad was effective.  Moved r1.
--                   tag_type logic to end to remove "T" if manually selected.
--   10/9/97 - ggw - Modified to support Hundred day tag type
--   12/2/97 - ggw - batch_no is now set by an passed argument
--   12/29/97 -ggw - Support 100 Day type, auto size and/or auto type
--   12/30/97 -ggw - Used to make tagrawfm.4gl.  Change all itemmore.price,
--                   nfor_price, and nfor_unt to item.(file).  Also, change
--                   eff_tag_type only during ad_calculations
#    3/23/98  -ggw - Added support for itembatch.type_change="A" for Add

#    10/13/98 -ggw - Use subdepartment 999 as discontinued item.  Change order
#                    number to DISCO if subdepartment=999.
#    2/22/99  -ggw - Add Aisle Field to extract
#    10/26/00 -ggw - Add suport for ad_type L for limit, p for po, c for compare basket
#    11/06/00 -ggw - Raley's Support, added vendor, ad_color, ad_start,reg_price
#                    ad_end
#    9/17/01  -ggw - ad limit field support
#    11/5/01  -jtp - added prompt for tag order - UPC or Dept./SubDept.
#    1/20/01  -ggw - Single program support for New Mexico Raleys and Food Source
#    3/05/03  -ggw - Add support for weekly movement
#    10/08/03 -jtp - Lump all ad types together for Food Source
#    10/08/03 -jtp - Removed support of IBM Regsiter pricing
#    11/22/04 -jtp - Change movement field to always be cur_unt
#    06/05/07 -jtp - Added default tag print sort order
#    12/17/09 -jtp - Change "Hundred Day" to "As Advertised"
#    2018-07-26 - ggw - Split support for ad_type = 1 on ad_stock and all other ad_types to tpr_stock for 32up, 6up and 2up 
DATABASE sabre

GLOBALS
  DEFINE num_tags char(1)
  DEFINE tmp_tag_size SMALLINT
  DEFINE tmp_tag_type CHAR(1)
  DEFINE tag_size_sel char(1)
  DEFINE tag_type_sel char(1)
  DEFINE batch_no integer
  DEFINE file_out char(20)
  DEFINE current_price char(20)
  DEFINE chain char(1)
  DEFINE reg_type char(1)
  DEFINE def_sort char(1)
  DEFINE cursor_txt char(600)
  DEFINE tmp_int SMALLINT
END GLOBALS

MAIN
  DEFINE dept_rep integer
  DEFINE tans char(1)        
  DEFINE itm_code_const integer
  DEFINE subdept_start smallint
  DEFINE subdept_end smallint
  DEFINE teffdate date
  DEFINE print_order char(1)
  DEFINE mvmt_field char(16)
  DEFINE mvmt_flag integer
    DEFINE item_rec
      RECORD
      tag_size like itemmore.tag_qty,  #tag size field
      tag_type like itemmore.tag_size, #char(1) force tag type field
      tag_quan like itemmore.tag_quan,
      whse     like itemmore.whse,
      order_no like itemmore.order_no,
      itm_code like itemmore.itm_code,
      dept     like itemmore.department,
      sub      like itemmore.subdepartment,
      price    like itemmore.price,
      nfor_price like itemmore.nfor_price,
      nfor_unt like itemmore.nfor_unt,
      ad_price like itemmore.ad_price,
      ad_nfor_unt like itemmore.ad_nfor_unt,
      ad_nfor_price like itemmore.ad_nfor_price,
      ad_start like itemmore.ad_start,
      ad_end like itemmore.ad_end,
      ad_type   like itemmore.ad_type,
      ad_limit like itemmore.ad_limit,
      desc_30  like itemmore.desc_30,
      desc_sign2 like itemmore.desc_sign2,
      item_pack like itemmore.item_pack,
      item_size like itemmore.item_size,
      item_meas like itemmore.item_meas,
      case_cost like itemmore.case_cost,
      case_pack like itemmore.case_pack,
      smprice   like itemmore.smprice,
      type_change like itembatch.type_change,
      aisle like itemmore.aisle,
      tag_color like itemmore.tag_color,
      vendor_code like itemmore.vendor_code,
      crv_amt like itemmore.crv_amt,
      description char(16),
      cur_price money(6,2),
      cur_nfor_price money(6,2),
      cur_nfor_unt integer,
      mvmt_last_week integer
    END RECORD
  LET batch_no=arg_val(1)
  LET chain=arg_val(2)
  LET reg_type=arg_val(3)
  LET def_sort=arg_val(4)

  LET file_out="tagraw",batch_no using "<<<<",".txt"
  DISPLAY " "
  LET teffdate=today
  DISPLAY "Default effective date is  : ",teffdate
  DISPLAY " "
  PROMPT "Please enter effective date for Tags : " for  teffdate
  DISPLAY ""
  IF teffdate >today+5 OR teffdate< today-5
  THEN DISPLAY "WARNING: effective date is more than 5 days off from current"
    DISPLAY ""
    PROMPT "ARE YOU SURE YOU WANT TO CONTINUE? (Y/N): " for tans
    IF NOT (tans="y" OR tans="Y")
      THEN
	DISPLAY "EXITING PROGRAM"
	EXIT PROGRAM
    END IF
  END IF 

IF teffdate is null
  THEN 
    DISPLAY "Setting effective date to today"
    LET teffdate=today
END IF

PROMPT "What Size Tag do you want? (Auto, Large, Small, Tiny, siGn): " for tag_size_sel

IF tag_size_sel IS NULL OR tag_size_sel=" " OR tag_size_sel="a" THEN
   LET tag_size_sel="A"
END IF
IF tag_size_sel ="A" or tag_size_sel = "a"
  THEN 
    DISPLAY "AUTO SIZING...."
END IF
#  Check for auto type
DISPLAY " "
DISPLAY "  Entering through the Ad will have tags follow the Ad flags."
DISPLAY " "
PROMPT "Print tags as type ([A]uto,[5]TPR/[1]Advertised/[R]egular): " for tag_type_sel

IF tag_type_sel IS NULL OR tag_type_sel="" THEN
   LET tag_type_sel="A"
END IF

IF def_sort=" " OR def_sort IS NULL THEN 
  LET def_sort="A"
END IF
PROMPT "Order of tags to print (A)isle/(U)pc/(C)ategory (Default=",def_sort,"): " FOR print_order
IF (print_order != "U" AND print_order !="u" AND
    print_order != "C" AND print_order !="c" AND
    print_order != "A" AND print_order !="a") OR
    print_order IS NULL THEN
  LET print_order = def_sort
END IF

PROMPT "Number of tags to print (D)efault/(#)specify (Default=D): " FOR num_tags
IF num_tags=" " OR num_tags IS NULL THEN
  LET num_tags="D"
END IF

CASE
  WHEN tag_size_sel="A" or tag_size_sel="a"
    DISPLAY "Printing Auto Size tags...."
  WHEN tag_size_sel="L" or tag_size_sel="l"
    DISPLAY "Printing Large Size tags...."
    LET tmp_tag_size=1
  WHEN tag_size_sel="S" or tag_size_sel="s"
    DISPLAY "Printing Small Size tags...."
    LET tmp_tag_size=2
  WHEN tag_size_sel="T" or tag_size_sel="t"
    DISPLAY "Printing Tiny Size tags...."
    LET tmp_tag_size=4
  WHEN tag_size_sel="G" or tag_size_sel="g"
    DISPLAY "Printing siGns ...."
    LET tmp_tag_size=8
END CASE
CASE
  WHEN tag_type_sel ="A" or tag_type_sel="a"
    DISPLAY "  with Auto Type Selection"
  WHEN tag_type_sel ="5" 
    DISPLAY "  with TPR Selection"
  WHEN tag_type_sel ="1" 
    DISPLAY "  with As Advertised Day Selection"
  WHEN tag_type_sel ="R" or tag_type_sel="r"
    DISPLAY "  with Regular (Every day Low price) Selection"
END CASE
  
DISPLAY " "
PROMPT "Press 'Q' to Quit or any other key to continue:" for tans
IF tans="q" or tans="Q"
  THEN EXIT PROGRAM
END IF
  
DISPLAY "Creating tag file for printer.  Please Wait...."

LET cursor_txt="SELECT ",
    "tag_qty,",
    "tag_size,",
    "tag_quan,",
    "whse,",
    "order_no,",
    "a.itm_code,",
    "a.department dept,",
    "a.subdepartment sub,",
    "a.price,",
    "a.nfor_price,",
    "a.nfor_unt,",
    "ad_price,",
    "ad_nfor_unt,",
    "ad_nfor_price,",
    "ad_start,",
    "ad_end,",
    "ad_type,",
    "ad_limit,",
    "desc_30 tagdesc,",
    "desc_sign2,",
    "item_pack,",
    "item_size,",
    "item_meas,",
    "a.case_cost,",
    "a.case_pack,",
    "smprice,",
    "type_change,",
    "a.aisle,",
    "tag_color,",
    "vendor_code,",
    "crv_amt "
    #DISPLAY cursor_txt
    #DISPLAY chain
    CASE 
    WHEN chain="R" #IBM Register
      LET cursor_txt=cursor_txt CLIPPED," ",
      "FROM itemmore a,itembatch b ",
      "WHERE b.batch=",batch_no USING "<<<<"," AND b.itm_code=a.itm_code"
    WHEN chain="F" #Innovax Register
      #3/5/03 ggw Movement support on tags
      #SELECT flg into mvmt_flag from sys
      #11/22/04 -jtp - Change movement field to always be cur_unt
      #IF mvmt_flag>=16384 THEN
      #  LET mvmt_field="cur_unt"
      #ELSE
      #  LET mvmt_field="prev_unt"
      #END IF
      LET mvmt_field="cur_unt"
      LET cursor_txt=cursor_txt CLIPPED," ",
      ",c.description,",
      "c.price,", 
      "c.nfor_price,",
      "c.nfor_unt,",
      "c.",mvmt_field CLIPPED," ",
      "FROM itemmore a,itembatch b, OUTER item c ",
      "WHERE b.batch=",batch_no USING "<<<<"," AND b.itm_code=a.itm_code AND a.itm_code=c.itm_code "
      #DISPLAY "cursor_txt:",cursor_txt
      #PROMPT "CONTINUE? " for tans
    END CASE 

CASE
  WHEN print_order="U" or print_order="u"
    LET cursor_txt=cursor_txt CLIPPED," ORDER BY a.itm_code"
  WHEN print_order="C" or print_order="c"
    LET cursor_txt=cursor_txt CLIPPED," ORDER BY dept,sub,a.itm_code"
  OTHERWISE
    LET cursor_txt=cursor_txt CLIPPED," ORDER BY aisle,dept,sub,a.itm_code"
END CASE

#DISPLAY cursor_txt
PREPARE prep_txt FROM cursor_txt
DECLARE item_cur CURSOR FOR prep_txt


START REPORT pricebook

FOREACH item_cur INTO item_rec.*
  IF reg_type="N" THEN     # NCR Registers
    LET tmp_int=length(item_rec.itm_code)+1
    # ggw 10/13/2011 commented out below to speed testing 
    CALL ncr_price(item_rec.itm_code,tmp_int)
    RETURNING item_rec.cur_price,item_rec.cur_nfor_price,item_rec.cur_nfor_unt
  END IF

  IF item_rec.ad_start > teffdate OR item_rec.ad_end < teffdate THEN
    LET item_rec.ad_start=""
    LET item_rec.ad_end=""
    LET item_rec.ad_price=""
    LET item_rec.ad_nfor_price=""
    LET item_rec.ad_nfor_unt=""
  END IF

  IF item_rec.tag_size IS NULL THEN
     LET item_rec.tag_size = 1
  END IF
  IF tag_size_sel = "A" THEN
     # Automatic Tag Sizing #
     LET tmp_tag_size=item_rec.tag_size
     WHILE tmp_tag_size > 0
        IF tmp_tag_size >= 8 THEN
           LET item_rec.tag_size=8
           LET tmp_tag_size = tmp_tag_size - 8
        ELSE
           IF tmp_tag_size >= 4 THEN
              LET item_rec.tag_size=4
              LET tmp_tag_size = tmp_tag_size - 4
           ELSE
              IF tmp_tag_size >= 2 THEN
                 LET item_rec.tag_size=2
                 LET tmp_tag_size = tmp_tag_size - 2
              ELSE
                 LET item_rec.tag_size=1
                 LET tmp_tag_size = 0
              END IF
           END IF
        END IF
        OUTPUT TO REPORT pricebook (item_rec.*,teffdate)
     END WHILE
  ELSE
     LET item_rec.tag_size=tmp_tag_size
     OUTPUT TO REPORT pricebook (item_rec.*,teffdate)
  END IF

END FOREACH
#FINISH REPORT pricebook # Don't print extra cr/lf

END MAIN

REPORT pricebook(r1)

  DEFINE r1
    RECORD
    tag_size like itemmore.tag_qty, #numeric
    tag_type like itemmore.tag_size,#char(1) Force tag type field
    tag_quan like itemmore.tag_quan,
    whse     like itemmore.whse,
    order_no like itemmore.order_no,
    itm_code like itemmore.itm_code,
    dept     like itemmore.department,
    sub      like itemmore.subdepartment,
    price    like itemmore.price,
    nfor_price like itemmore.nfor_price,
    nfor_unt like itemmore.nfor_unt,
    ad_price like itemmore.ad_price,
    ad_nfor_unt like itemmore.ad_nfor_unt,
    ad_nfor_price like itemmore.ad_nfor_price,
    ad_start like itemmore.ad_start,
    ad_end like itemmore.ad_end,
    ad_type like itemmore.ad_type,
    ad_limit like itemmore.ad_limit,
    desc_30  like itemmore.desc_30,
    desc_sign2 like itemmore.desc_sign2,
    item_pack like itemmore.item_pack,
    item_size like itemmore.item_size,
    item_meas like itemmore.item_meas,
    case_cost like itemmore.case_cost,
    case_pack like itemmore.case_pack,
    smprice   like itemmore.smprice,
    type_change like itembatch.type_change,
    aisle like itemmore.aisle,
    tag_color like itemmore.tag_color,
    vendor_code like itemmore.vendor_code,
    crv_amt like itemmore.crv_amt,
    description char(16),
    cur_price  money(12,2),
    cur_nfor_price  money(12,2),
    cur_nfor_unt  smallint,
    mvmt_last_week integer,
    teffdate date 
  END RECORD,

  esc char(1),
  cent char(1),
  titm_code char(13),
  rep_date date,
  tot_price decimal(10,2),
  tot_costwa decimal(10,2),
  tot_cost  decimal(10,2),
  eff_price decimal(10,2),
  eff_nfor_price decimal(10,2),
  eff_nfor_unt integer,
  eff_tag_type char(1),
  tcase_cost decimal(10,2)

  OUTPUT 
    top margin 0
    bottom margin 0
    left margin 0
    right margin 174
    page length 100
  #  REPORT TO "tagraw.txt"
    REPORT TO file_out
  FORMAT 
  

  on every row 

    -- CALCULATIONS 
CASE 
WHEN tag_size_sel="L" or tag_size_sel="l"
  LET r1.tag_size=1
WHEN tag_size_sel="S" or tag_size_sel="s"
  LET r1.tag_size=2
WHEN tag_size_sel="T" or tag_size_sel="t"
  LET r1.tag_size=4
WHEN tag_size_sel="G" or tag_size_sel="g"
  LET r1.tag_size=8
END CASE

-- begmod -ggw - 2/18/97  - ad calculations
  ##  Set eff_tag_type based on ad calculation
  ##  Set eff_tag_type based on r1.tag_type
  ##     Then set eff_tag_type based on manualy entered tag_type_sel
  ##  Raleys = Default tag type is tiny if no ad, if ad then set to ad_type
  CASE
    WHEN r1.ad_start is not null and r1.ad_end is not null AND 
        (r1.ad_price is not null or r1.ad_nfor_price is not null) AND
        (r1.ad_start <= r1.teffdate and r1.ad_end >= r1.teffdate)
      LET eff_price=r1.ad_price
      LET eff_nfor_price=r1.ad_nfor_price
      LET eff_nfor_unt=r1.ad_nfor_unt
      IF chain="R" THEN
        LET eff_tag_type= r1.ad_type 
      ELSE
        LET eff_tag_type= r1.ad_type 
      END IF
                                 
    OTHERWISE
      LET eff_price=r1.price
      LET eff_nfor_price=r1.nfor_price
      LET eff_nfor_unt=r1.nfor_unt
      LET eff_tag_type=r1.tag_type
  END CASE   
  -- endmod
  #  r1.tag_type takes precedence over effective r1.ad_type field
  IF r1.tag_type is not null and r1.tag_type!=" "
    THEN
    LET eff_tag_type=r1.tag_type
  END IF

# begmod - ggw - 4/28/97 - Set r1.tag_type(Regular, TPR, Advertised ) based
#          on tag_type_selection
IF not (tag_type_sel="A" or tag_type_sel="a") THEN
  CASE 
    WHEN tag_type_sel="5"
      LET eff_tag_type="5"
    WHEN tag_type_sel="1"
      LET eff_tag_type="1"
    WHEN (tag_type_sel="R" or tag_type_sel="r")
      LET eff_tag_type=" "
    OTHERWISE
      # Follow r1.tag_type as entered in table
  END CASE 
END IF
## endmod - ggw - 4/28/97

## begmod -ggw - 10/13/98  - discontine subdept 999
IF r1.sub = 999
  THEN LET r1.order_no="DISCO"
END IF
## endmod -ggw - 10/13/98  - discontine subdept 999


LET tcase_cost=(r1.case_cost/r1.case_pack)*100
IF r1.type_change is null or r1.type_change=1 or r1.type_change="A" THEN
  IF num_tags!="D" THEN
    LET r1.tag_quan=num_tags
  END IF
  print 
    r1.tag_size using "<<<<","|",
    eff_tag_type CLIPPED,"|",
    r1.tag_quan using "---","|",
    r1.whse using "<<<<","|",
    r1.order_no CLIPPED,"|",
    r1.itm_code CLIPPED,"|",
    r1.dept using "<<<"   , "|",
    r1.sub  using "<<<<<<"   ,"|", 
    eff_price using "<<<.##","|", 
    eff_nfor_price using "<<<.##","|", 
    eff_nfor_unt using "<<<","|", 
    r1.desc_30 CLIPPED,"|", 
    r1.desc_sign2 CLIPPED,"|", 
    r1.item_pack using "<<<<" ,"|",
    r1.item_size using "<<<<<.<<<<","|", 
    r1.item_meas CLIPPED,"|", 
    tcase_cost USING "&&&&&","|", 
    r1.case_pack using "<<<<","|", 
    r1.smprice ,"|",
    r1.description CLIPPED,"|", 
    r1.cur_price using "<<<.##","|",
    r1.cur_nfor_price using "<<<.##","|",
    r1.cur_nfor_unt using "##","|",
    r1.ad_start using "mm/dd/yy" ,"|",
    r1.ad_end using "mm/dd/yy","|",
    r1.ad_limit using "###","|",
    r1.aisle CLIPPED,"|",
    r1.tag_color CLIPPED,"|",
    r1.vendor_code CLIPPED,"|",
    r1.crv_amt using "<.##","|",
    #Following added for Regular Pricing of Ad items
    r1.price using "<<<.##","|",
    r1.nfor_unt using "##","|",
    r1.nfor_price using "<<<.##","|",
    r1.mvmt_last_week using "#####","|"
END IF
ON LAST ROW
  print " "
END REPORT

