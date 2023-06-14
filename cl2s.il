;============================================================================
; Copyright Tait Electronics Ltd., 2005. All rights reserved.
;
; Program Name: clineToShape_public.il
;
; Author:				Dave Elder, PCB Design Manager,
;								Tait Electronics, Christchurch, New Zealand
;								dave.elder@taitradio.com
;
; Tested on:		Allegro 16.5. Will not work on earlier versions than 16.3.
;
; Purpose:			Convert a cline or line to a shape.
;
; Usage:				To run the routine within Allegro type: "cl2s".
;
; History:
;
; 2.1	11-Jul-11 DE	Public release.
;
;============================================================================

axlCmdRegister("cl2s" 'CL2S_clineToShape ?cmdType "interactive")
;load(strcat(eval('mySkillPath), "\\clineToShape_public.il"))
;AFn_lint("clineToShape_public.il")

defun( CL2S_clineToShape ()
 let((mark, popupAllActive, popupGrayOops, oldSetData, endType, cnrStyl, pt, lines, line, layer, netName, parent, polys, path, segs, (oopsCount 0))
	mark = axlDBTransactionStart()
	popupAllActive = axlUIPopupDefine(nil '(
		("Done" "axlDBTransactionCommit(mark), axlFinishEnterFun()")
		("Oops" "axlDBTransactionOops(mark), when(zerop(--oopsCount), axlUIPopupSet(popupGrayOops))")
		("Cancel" "axlDBTransactionRollback(mark), axlCancelEnterFun()")
		("MENU_SEPARATOR", nil)
		("Toggle End Cap Type", "endType = if(endType == 'ROUND, 'SQUARE, 'ROUND), printf(\"%s\\n\", CL2S_setPrompt(endType, cnrStyl))")
		("Toggle Vertex Style", "cnrStyl = if(cnrStyl == 'ROUNDED, 'SHARP, 'ROUNDED), printf(\"%s\\n\", CL2S_setPrompt(endType, cnrStyl))")
	))
	axlUICmdPopupSet(popupAllActive)
	popupGrayOops = axlUIPopupDefine(nil, '(
		("Done" "axlDBTransactionCommit(mark), axlFinishEnterFun()")
		("Cancel" "axlDBTransactionRollback(mark), axlCancelEnterFun()")
		("Toggle End Cap Type", "endType = if(endType == 'ROUND, 'SQUARE, 'ROUND), printf(\"%s\\n\", CL2S_setPrompt(endType, cnrStyl))")
		("Toggle Vertex Style", "cnrStyl = if(cnrStyl == 'ROUNDED, 'SHARP, 'ROUNDED), printf(\"%s\\n\", CL2S_setPrompt(endType, cnrStyl))")
	))
	axlUIPopupSet(popupGrayOops)
	endType = 'ROUND
	cnrStyl = 'ROUNDED
	oldSetData = list(axlGetSelSet(), axlGetFindFilter(nil), axlGetFindFilter(t))
	axlSetFindFilter(?enabled list("noall", "LINES", "CLINES"), ?onButtons list("noall", "CLINES", "LINES"))
	while(pt = axlEnterPoint(?prompts CL2S_setPrompt(endType, cnrStyl))
		axlClearSelSet()
		while(axlAddSelectPoint(pt), t); necessary to add all objects at point
		lines = axlGetSelSet()
		caseq(length(lines)
			(0, axlUIWPrint(nil, 'error, "No cline/line (without arc segments) here"))
			(1
				line = car(lines), layer = line ->layer, netName = line ->net ->name, parent = line ->parent
				polys = axlPolyOperation(nil, if(cnrStyl == 'SHARP
					 then axlPolyFromDB(line ?endCapType 'ROUND) else mapcar(lambda((seg), car(axlPolyFromDB(seg ?endCapType 'ROUND))), line ->segments)), 'OR)
				when(polys && endType == 'SQUARE
					path = axlDB2Path(line); >= 16.3 only
					segs = axlPathGetPathSegs(path)
					polys = CL2S_squareLineEnds(segs, polys, nil)
					polys = CL2S_squareLineEnds(reverse(segs), polys, t)
				)
				if(polys then
					axlDBTransactionMark(mark), axlUIPopupSet(popupAllActive), oopsCount++
					axlClearSelSet(); Require for Allegro transaction bug that does not clear the find bit (Courtesy fxf).
					axlDeleteObject(line)
					foreach(poly, polys, axlDBCreateShape(poly, t, layer, netName, and(parent ->objTye, parent)))
				 else axlUIWPrint(nil, 'error, "Error. Could not convert this cline/line.")
				)
			)
			(t, axlUIWPrint(nil, 'error, "Multiple clines/lines Here. Pick again"))
		); case
	); while
	axlClearDynamics()
	axlUICmdPopupSet(nil)
	axlSetFindFilter(?enabled cons("noall", cadr(oldSetData)) ?onButtons cons("noall", caddr(oldSetData)))
	axlSingleSelectObject(car(oldSetData))
	axlUIWUpdate(nil)
))

defun( CL2S_squareLineEnds (segs, polys, reversePts)
 let((seg1, seg2, endPt, r, cpt, x, y, ang, cw, pts, rPts, lPts, path, poly)
	seg1 = car(segs)
	seg2 = cadr(segs)
	endPt = axlPathSegGetEndPoint(seg1)
	r = axlPathSegGetWidth(seg1) / 2.0
	cpt = axlPathSegGetArcCenter(if(reversePts, seg1, seg2))
	if(cpt then
		x = car(endPt) - car(cpt)
		y = cadr(endPt) - cadr(cpt)
		ang = CL2S_rad2deg(atan2(y, x))	
		x = car(axlGeoRotatePt(-ang, x:y, nil))
		cw = axlPathSegGetArcClockwise(seg1)
		pts = list(endPt, axlMXYAdd(cpt, axlGeoRotatePt(ang, list(x, if(cw, -r, r)), nil)))
	 else
		pts = CL2S_stretchLine(endPt, axlPathSegGetEndPoint(seg2), r)
	)
	rPts = CL2S_lineOffset(car(pts), cadr(pts), r)
	lPts = CL2S_lineOffset(car(pts), cadr(pts), -r)
	path = axlPathStart(list(car(rPts), cadr(rPts), cadr(lPts), car(lPts), car(rPts)))
	poly = axlPolyFromDB(path ?endCapType 'SQUARE)
	axlPolyOperation(poly, polys, 'OR)
))

defun( CL2S_setPrompt (endType, cnrStyl)
	CL2S_cursorDynamics(endType, cnrStyl)
	sprintf(nil, "Select cline or line (End Cap Type will be %s, Vertex Style will be %s)", endType, cnrStyl)
)

defun( CL2S_cursorDynamics (endType, cnrStyl)
 let((exts, r, rpath, x, y, dy)
	axlClearDynamics()
	;Add shape image graphics:
	exts = axlWindowBoxGet(), r = min(caadr(exts) - caar(exts), cadadr(exts) - cadar(exts)) / 40.0
	x = y = r / sqrt(2)
	dy = r * sqrt(2)
	rpath = axlPathStart(list(0:-dy), 0)
	axlPathLine(rpath, nil, list(x, -(dy + y)))
	axlPathArcAngle(rpath, nil, list(3 * x, -y), nil, 180.0)
	axlPathLine(rpath, nil, list(x, y))
	axlPathArcAngle(rpath, nil, list(-x, y), nil, 90.0)
	axlPathLine(rpath, nil, list(-3 * x, -y))
	axlPathArcAngle(rpath, nil, list(-x, -(dy + y)), nil, 180.0)
	axlPathLine(rpath, nil, 0:-dy)
	axlAddSimpleMoveDynamics(0:0, rpath, "path", ?color axlDBControl('activeLayer))

	r = r * 1.02; Make the overlayed graphics slightly larger to avoid colour conflicts that can blank the graphics.
	x = y = r / sqrt(2)
	dy = r * sqrt(2)
	cond(
		(endType == 'ROUND && cnrStyl == 'ROUNDED
			rpath = axlPathStart(list(0:-dy), 0)
			axlPathLine(rpath, nil, list(x, -(dy + y)))
			axlPathArcAngle(rpath, nil, list(3 * x, -y), nil, 180.0)
			axlPathLine(rpath, nil, list(x, y))
			axlPathArcAngle(rpath, nil, list(-x, y), nil, 90.0)
			axlPathLine(rpath, nil, list(-3 * x, -y))
			axlPathArcAngle(rpath, nil, list(-x, -(dy + y)), nil, 180.0)
			axlPathLine(rpath, nil, 0:-dy)
		)
		(endType == 'ROUND && cnrStyl == 'SHARP
			rpath = axlPathStart(list(0:-dy), 0)
			axlPathLine(rpath, nil, list(x, -(dy + y)))
			axlPathArcAngle(rpath, nil, list(3 * x, -y), nil, 180.0)
			axlPathLine(rpath, nil, list(0, dy))
			axlPathLine(rpath, nil, list(-3 * x, -y))
			axlPathArcAngle(rpath, nil, list(-x, -(dy + y)), nil, 180.0)
			axlPathLine(rpath, nil, 0:-dy)
		)
		(endType == 'SQUARE && cnrStyl == 'ROUNDED
			rpath = axlPathStart(list(0:-dy), 0)
			axlPathLine(rpath, nil, list(2 * x, -(dy + 2 * y)))
			axlPathLine(rpath, nil, list(4 * x, -dy))
			axlPathLine(rpath, nil, list(x, y))
			axlPathArcAngle(rpath, nil, list(-x, y), nil, 90.0)
			axlPathLine(rpath, nil, list(-4 * x, -dy))
			axlPathLine(rpath, nil, list(-2 * x, -(dy + 2 * y)))
			axlPathLine(rpath, nil, 0:-dy)
		)
		(endType == 'SQUARE && cnrStyl == 'SHARP
			rpath = axlPathStart(list(0:-dy), 0)
			axlPathLine(rpath, nil, list(2 * x, -(dy + 2 * y)))
			axlPathLine(rpath, nil, list(4 * x, -dy))
			axlPathLine(rpath, nil, list(0, dy))
			axlPathLine(rpath, nil, list(-4 * x, -dy))
			axlPathLine(rpath, nil, list(-2 * x, -(dy + 2 * y)))
			axlPathLine(rpath, nil, 0:-dy)
		)
	)
	axlAddSimpleMoveDynamics(0:0, rpath, "path")
))

defun( CL2S_rad2deg (radians) radians * 90.0 / asin(1))

defun( CL2S_stretchLine (pt1, pt2, d1 @optional d2)
 let((ends)
  ends = CL2S_truncateLineSegment(pt2, pt1, -d1)
  when(ends && d2, ends = CL2S_truncateLineSegment(cadr(ends), car(ends), -d2))
  ends
))

defun( CL2S_truncateLineSegment (pt1, pt2, shorten_len)
 let((x1, y1, x2, y2, len, diff_len)
   x1 = car(pt1), y1 = cadr(pt1), x2 = car(pt2), y2 = cadr(pt2)
   len = CL2S_lineLength(x1, y1, x2, y2)
   when(and(!axlGeoEqual(float(shorten_len), 0.0), len > shorten_len)
     diff_len = len - shorten_len
     x2 = x1 + diff_len * (x2 - x1) / len
     y2 = y1 + diff_len * (y2 - y1) / len
   )
   and(len > shorten_len, list(x1:y1, x2:y2))
))

defun( CL2S_lineLength (x1, y1, x2, y2)
 let((Dx, Dy, I_factor)
  Dx = abs(x2 - x1)
  Dy = abs(y2 - y1)
  I_factor = float(max(abs(Dx), abs(Dy), 1))
  I_factor * sqrt(expt((Dx / I_factor) 2) + expt((Dy / I_factor) 2))
))

defun( CL2S_lineOffset (pt1, pt2, dist @optional reversePoints) ; positive dist is right side, negative is left
 let((x1, y1, x2, y2, f3, cw, bigArc, ang, cpt, x0, y0, dx1, dy1, theta, dx2, dy2, line, m0, b0, mp, bp, mp2, bp2, m2, b2, radius)
  x1 = car(pt1), y1 = cadr(pt1), x2 = car(pt2), y2 = cadr(pt2), f3 = caddr(pt2), cw = cadddr(pt2), bigArc = nth(4, pt2)
  caseq(length(pt2)
    (2
			dist = dist * (cond
				(y1 > y2 && x1 == x2,								-1) ;MAINTAIN THIS ORDER
    		(y2 > y1 && (x2 < x1 || x2 == x1),   1)
    		((y2 < y1 || y2 == y1) && x1 > x2,   1)
    		((y2 < y1 || y2 == y1) && x2 > x1,  -1)
    		(y2 > y1 && x2 > x1,								-1)
  		)
      line = CL2S_lineEquation(x1, y1, x2, y2), m0 = car(line), b0 = cadr(line)
      line = CL2S_linePerpendicular(m0, x1, y1), mp2 = car(line), bp2 = cadr(line)
      line = CL2S_lineParallel(m0, b0, dist), m2 = car(line), b2 = cadr(line)
      line = CL2S_linePerpendicular(m0, x2, y2) mp = car(line), bp = cadr(line)
      pt1 = CL2S_lineLineIntersect(mp2, bp2, m2, b2), pt2 = CL2S_lineLineIntersect(mp, bp, m2, b2)
    )
    (3, ang = f3, cpt = axlGeoArcCenterAngle(pt1, x2:y2, ang))
    (t
      radius = f3, cpt = axlGeoArcCenterRadius(pt1, x2:y2, radius, cw, bigArc)
      ang = -1 * CL2S_angleBetweenPtsWithRadius(pt1, x2:y2, radius, cw)
    )
  ); caseq
  if(line then if(reversePoints then list(pt2, pt1) else list(pt1, pt2))
   else
    unless(minusp(ang), dist = dist * -1)
    x0 = car(cpt), y0 = cadr(cpt)
    dx1 = x1 - x0, dy1 = y1 - y0, theta = atan2(dy1, dx1), dx1 = dx1 - dist * cos(theta), dy1 = dy1 - dist * sin(theta)
    dx2 = x2 - x0, dy2 = y2 - y0, theta = atan2(dy2, dx2), dx2 = dx2 - dist * cos(theta), dy2 = dy2 - dist * sin(theta)
    pt1 = list(x0 + dx1, y0 + dy1), pt2 = list(x0 + dx2, y0 + dy2)
    if(reversePoints then list(pt2, append1(pt1, -ang)) else list(pt1, append1(pt2, ang)))
  )
))

defun( CL2S_lineEquation (x1, y1, x2, y2)
 let((m, b)
  x1 = float(x1), y1 = float(y1), x2 = float(x2), y2 = float(y2)
  cond(
    (axlGeoEqual(x1, x2), list(nil, x1))
    (t, m = (y2 - y1) / (x2 - x1), b = y2 - m * x2, list(m, b))
  )
))

defun( CL2S_linePerpendicular (m1, x, y)
 let((m2, b2)
  cond(
    (!m1, list(0, y))
    (axlGeoEqual(m1, 0.0), list(nil, x))
    (t, m2 = -1.0 / float(m1), b2 = y - m2 * x, list(m2, b2))
  )
))

defun( CL2S_lineParallel (m1, b1, distance)
 let((m2, b2)
  cond(
    (!m1, list(nil, b1 + distance))
    (zerop(m1), list(0, b1 + distance))
    (t, m2 = m1, b2 = b1 + distance / cos(atan2(m1, 1)), list(m2 b2))
  )
))

defun( CL2S_lineLineIntersect (m1, b1, m2, b2)
 let((x)
  cond(
    (!m1 && !m2, nil)
    (!m1, list(b1, m2 * b1 + b2))
    (!m2, list(b2, m1 * b2 + b1))
    (m1 == m2, nil)
    (t, x = (b2 - b1) / (m1 - m2), list(x, m1 * x + b1))
  )
))

defun( CL2S_angleBetweenPtsWithRadius (pt2, pt1, r, cw)
  CL2S_radius2Angle(car(pt2) - car(pt1), cadr(pt2) - cadr(pt1), r) * if(cw, 1, -1)
)

defun( CL2S_radius2Angle (x, y, r)
  if(zerop(x) && zerop(y) then 0.0 else CL2S_rad2deg(2 * asin(CL2S_rndp(if(zerop(x), y / (2 * r * sin(atan2(y, x))), x / (2 * r * sin(atan2(x, y)))), 8))))
)

defun( CL2S_rndp (x @optional (dec 0))
  dec = fix(dec)
  round(x * expt(10, dec)) / float(expt(10, dec))
)