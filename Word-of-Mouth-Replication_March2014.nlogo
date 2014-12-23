globals 
[ 
  clock  
  sales  
  percent95 
  percent50 
  percent16   
  weak-contacts 
  ;;personal_network
  market-size
  size_of_personal_network
  size_of_weak_contacts
  strong-ties-effect
  weak-ties-effect
  advertising-effect
  ]

turtles-own
[
  my-group          ;; a # representing group membership , or -1 if not in a group.
  buy 
  influence 
  strong-effect 
  weak-effect 
  friends 
  acquaintances    
;;  advertising-effect             ;; replaced by slider  
;;  size_of_weak_contacts         ;; replaced by slider
;;  size_of_personal_network       ;; replaced by slider
;;  strong-ties-effect             ;; replaced by slider 
;;  weak-ties-effect               ;; replaced by slider 
]

;;; sets up the model
to setup
  clear-all
  ask patches [ set pcolor white ]
  set market-size 2 ^ market2
  set size_of_personal_network 2 ^ nStrong2
  set size_of_weak_contacts 2 ^ nWeak2
  set strong-ties-effect 2 ^ StrongEffect
  set weak-ties-effect 2 ^ WeakEffect
  set advertising-effect 2 ^ AdEffect
  crt market-size
  [
;;    set shape "person"
    set shape "face sad"
    set size 1
    set color cyan
    ;; randomly place them initially
    setxy random-xcor random-ycor
    set heading random-float 360
    ;; turtles start out ungrouped
    set my-group -1
   ]    

 assign-by-size 
 set clock 0
 ;; color is related to CLOSE TIES group.
 ask turtles
   [
     set buy 0  
     set color scale-color cyan my-group ((max [my-group] of turtles ) / 4 ) ((max [my-group] of turtles) * 1.2 ) 
     set friends nearby
   ]
   ask turtle 1
   [
     set buy 1
     set color red
     set shape "face happy"
     set size 2
   ]
   gather-turtles
   reset-ticks
end

;;; this procedure randomly assigns turtles to groups based on the desired
;;; size of the groups. all the groups will have the desired size except for
;;; at most one group, which contains the remainder of the turtles. more
;;; formally, if there are n turtles, and the desired group size is k, this
;;; procedure will produce j = floor (n / k) groups of k turtles, and if
;;; n mod k > 0, it will produce one group of n mod k turtles.
;;;
;;; This section of code adapted from the GROUPING TURTLES EXAMPLE from the MODELS LIBRARY
;;; *** NetLogo 4.0beta5 Code Example Copyright Notice ***
;;; (C) 2004 Uri Wilensky.

to assign-by-size
  ;; all turtles are initially ungrouped
  ask turtles [ set my-group -1 ]
  let unassigned turtles
  ;; start with group 0 and loop to build each group
  let current 0
  while [any? unassigned]
  [
    ;; place a randomly chosen set of size_of_personal_network turtles into the current
    ;; group. or, if there are less than size_of_personal_network turtles left, place the
    ;; rest of the turtles in the current group.
    ask n-of (min (list size_of_personal_network (count unassigned))) unassigned
      [ set my-group current ]
    ;; consider the next group.
    set current current + 1
    ;; remove grouped turtles from the pool of turtles to assign
    set unassigned unassigned with [my-group = -1]
  ]
end

;;;
;;; GO PROCEDURES
;;;
to go
    gather-turtles
 if clock > 3000 [ finish stop ]  
;;  if not any? turtles with [ buy = 0 ] [ stop ] 
  if sales >= ( market-size * 0.95 )
   [ 
      finish 
      stop
   ]
    ask turtles
   [  if buy != 1
     [ likelihood ]
   ]
   set sales sum [ buy ] of turtles
   record-data
   do-plots
   set clock (clock + 1)  
end

;;;
;;; the following procedures are used to implement the visualization,
;;; and really don't have anything to do with grouping.
;;; unless you're particularly interested, it's safe to ignore the rest of the
;;; code.
;;;
;;;
;;; This section of code adapted from the GROUPING TURTLES EXAMPLE from the MODELS LIBRARY
;;; *** NetLogo 4.0beta5 Code Example Copyright Notice ***
;;; (C) 2004 Uri Wilensky.
;; causes the turtles to run around, going to their group's "home" if they're
;; in a group.

to gather-turtles
  ask turtles
  [
    ;; if i'm in a group, move towards "home" for my group
    if my-group != -1
      [ face get-home ]
    ;; wiggle a little and always move forward, to make sure turtles don't all
    ;; pile up
    fd  distance get-home
       lt random-float 90
       fd  size_of_personal_network / 10 
  ]
end

;; figures out the home patch for a group. this looks complicated, but the
;; idea is simple. we just want to lay the groups out in a regular grid,
;; evenly spaced throughout the world. we want the grid to be square, so in
;; some cases not all the positions are filled.
;;; This section of code adapted from the GROUPING TURTLES EXAMPLE from the MODELS LIBRARY
;;; *** NetLogo 4.0beta5 Code Example Copyright Notice ***
;;; (C) 2004 Uri Wilensky.

to-report get-home ;; turtle procedure
  ;; calculate the minimum length of each side of our grid
  let side ceiling (sqrt (max [my-group] of turtles + 1))
  report patch
           ;; compute the x coordinate
           (round ((world-width / side) * (my-group mod side)
             + min-pxcor + int (world-width / (side * 2))))
           ;; compute the y coordinate
           (round ((world-height / side) * int (my-group / side)
             + min-pycor + int (world-height / (side * 2))))
end


to-report nearby         ;; check with friends (strong ties) 
    report               ;; by defining turtles in my group
    turtles with [ my-group = [my-group] of myself ]
end


to likelihood
  set strong-effect sum [ buy ] of friends 
  set weak-effect sum [ buy ] of n-of size_of_weak_contacts turtles with [ my-group != myself ] 
  set influence 1 - (( 1 - advertising-effect ) * ( 1 - strong-ties-effect ) ^ strong-effect * ( 1 - weak-ties-effect ) ^ weak-effect ) 
  ifelse influence >= random-float 1
    [ set buy 1   set color orange   set shape "face happy"]
    [ set buy 0 ]
end 
  
to do-plots
  ;; draw horizontal lines to show milestones
  set-current-plot-pen "16 percent"
  auto-plot-off
  plotxy 0 16
  plotxy 200 16
  auto-plot-on
  
  set-current-plot-pen "50 percent"
  auto-plot-off
  plotxy 0 50
  plotxy 200 50
  auto-plot-on

  set-current-plot-pen "95 percent"
  auto-plot-off
  plotxy 0 95
  plotxy 200 95
  auto-plot-on
  
  set-current-plot "Market Penetration"
  set-current-plot-pen "sales"
  my-update-plots
end

to my-update-plots
  plotxy clock sales / market-size * 100
end

to record-data
  if sales <= market-size * 0.95
      [  set percent95 clock + 1 ]
  if sales <= market-size * 0.50
      [  set percent50 clock + 1 ]
  if sales <= market-size * 0.16 
      [ set percent16 clock + 1 ]
end

to finish
  type size_of_personal_network type "  "
  type size_of_weak_contacts type "  "
  type weak-ties-effect  type "  " 
  type strong-ties-effect  type "  " 
  type advertising-effect type "  " 
  type percent16  type "  " 
  type percent50  type "  " 
  type percent95  print " "
end
@#$#@#$#@
GRAPHICS-WINDOW
344
10
864
551
25
25
10.0
1
10
1
1
1
0
1
1
1
-25
25
-25
25
0
0
0
ticks
30.0

SLIDER
10
97
246
130
nStrong2
nStrong2
1
6
3
1
1
Power
HORIZONTAL

BUTTON
13
10
82
43
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
10
53
245
86
Market2
Market2
6
11
10
1
1
Power
HORIZONTAL

BUTTON
96
10
159
43
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
8
141
246
174
nWeak2
nWeak2
0
8
0
1
1
Power
HORIZONTAL

SLIDER
6
277
250
310
AdEffect
AdEffect
-12
-8
-12
1
1
NIL
HORIZONTAL

SLIDER
7
232
249
265
WeakEffect
WeakEffect
-12
-6
-8
1
1
NIL
HORIZONTAL

SLIDER
7
187
248
220
StrongEffect
StrongEffect
-8
-4
-4
1
1
NIL
HORIZONTAL

PLOT
7
315
333
478
Market Penetration
time
Percent
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"16 percent" 1.0 0 -11221820 true "" ""
"50 percent" 1.0 0 -11221820 true "" ""
"95 percent" 1.0 0 -11221820 true "" ""
"sales" 1.0 0 -16777216 true "" ""

BUTTON
178
13
241
46
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
7
480
77
525
Informed
sales / market-size * 100
1
1
11

MONITOR
275
480
332
525
clock
clock
0
1
11

MONITOR
247
42
337
87
Market Size
market-size
0
1
11

MONITOR
247
85
336
130
N Strong
size_of_personal_network
0
1
11

MONITOR
247
130
336
175
N Weak
size_of_weak_contacts
0
1
11

MONITOR
247
175
335
220
Strong Effect
strong-ties-effect
6
1
11

MONITOR
249
220
335
265
WeakEffect
weak-ties-effect
6
1
11

MONITOR
250
265
334
310
AdEffect
advertising-effect
8
1
11

@#$#@#$#@
## WHAT IS IT?

Word-of-Mouth-Replication is a replication of a study reported in:

Jacob Goldenberg, Barak Libai and Eitan Muller (2001), "Talk of the Network: A Complex Systems Look at the Underlying Process of Word-of-Mouth," Marketing Letters, (12), pp. 209-221.

--- quote ---  
The strength of weak ties  
There is scant evidence on the breakdown of the personal communication between closer and stronger communications that are within an individual's own personal group (strong ties), and weaker and less personal communications that an individual makes with a wide, often random, set of other acquaintances and colleagues (weak ties). We model these two phenomena and show that the influence of weak ties is at least as strong as the influence of strong ties. Despite the relative inferiority of the weak tie parameter in the model's assumptions (strong ties reflect greater probability for an individual-level transformation), their effect approximates or exceeds that of strong ties, in all of the process stages.  
--- unquote ---

## HOW IT WORKS

Five parameters control the diffusion of knowledge through a community:  
* Size of one's personal network - number close friends and family whom we trust  
* Size of one's loose contacts - number of acquaintances met and then perhaps forgotten  
* Salience of contact with the personal network  
* Salience of contact with loose contacts  
* Salience of advertising

In the early stages of new product diffusion then advertising clearly has the greatest influence (as no-one knows about it they can't talk about it), but word-of-mouth quickly takes over.

## HOW TO USE IT

Market Size adjusts the number of people in the market.  A very large number makes the model VERY slow with no significant improvement in consistency of output.  I suggest leaving it at about 1000.

Size of Personal Network is the number of people in constant, highly-regarded, contact.  It ranges from 5 to 29 consistent with Goldenberg et al.  A larger number implies faster communication within the clique.

Size of Weak Contacts is the number of people that each person speaks with in each period from outside the clique.  It ranges from 5 to 29 consistent with Goldenberg et al.  A larger number implies faster communication between cliques.

Weak Ties Effect is a parameter affecting the influence of the number of weak contacts on awareness likelihood.

Strong Ties Effect is a parameer affecting the influence of the number of strong contacts on awareness likelihood.

Advertising Effect is a parameter representing the level of non-Word-of-Mouth information received by all citizens in each time period.

## THINGS TO NOTICE

Grid lines in the graph show key points in the diffusion process:  
16% adoption is typically the "takeoff" point,  
50% adoption is typically a point of inflection,  
95% in this model is the point at which the model stops, having achieved near saturation.

Different parameter values will affect the time to takeoff (16%), time to inflection  (50%) and total time to saturation (95%).

When Strength-of-Weak-Ties is set low and Strength-of-Strong-Ties is set high then it is quite common to see near complete adoption in the community with one or two cliques having no adopters at all.  These would normally be called "LAGARDS" except that they are really not different from the other groups.

## THINGS TO TRY

Not surprisingly, the speed of diffusion is a function of number of contacts (strong and weak) and the saliency of those contacts (strong and weak), and of outside information sources (advertising).  The higher the number the shorter the time.

Goldenberg et.al. concluded with their simulation that weak effects were about equally as important as strong effects.  This simulation, however, suggests that they are only between one third and one half as important.  Important but not nearly as important as claimed in the original paper.

## EXTENDING THE MODEL

Purely for cosmetics it might be nice to move the turtles into groups before starting the model - with rapid diffusion often the turtles are not well grouped (graphically, but they are okay WRT communication.) 

Advertising is presented as having a constant effect in each time period.  The advertising influence may be better presented as a cumulative effect (with appropriate decay function), although advertising influence is really only a "starter" function in this model which is designed to measure the relative effect of strong and weak ties.

## NETLOGO FEATURES

RANDOM-FLOAT function is used in an IFELSE routine to decide if a turtle has purchased (1) or not (0).  
N-OF function selects a new random sample of turtles not in my clique.  
And of course the graphics would have been impossible, or much poorer if not for the lovely Grouping Turtles Example by Uri Wilensky.

## RELATED MODELS

 GROUPING TURTLES EXAMPLE in the MODELS LIBRARY
 *** NetLogo 4.0beta5 Code Example Copyright Notice ***
 (C) 2004 Uri Wilensky.

## CREDITS AND REFERENCES

Jacob Goldenberg, Barak Libai and Eitan Muller (2001), "Talk of the Network: A Complex Systems Look at the Underlying Process of Word-of-Mouth," Marketing Letters, (12), pp. 209-221.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.5
@#$#@#$#@
set number-of-groups 25
setup
assign-by-number
repeat 25 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Goldenberg_replication May 2012 2" repetitions="3" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>percent16</metric>
    <metric>percent50</metric>
    <metric>percent95</metric>
    <enumeratedValueSet variable="size_of_personal_network">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_weak_contacts">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strong-ties-effect">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weak-ties-effect">
      <value value="0.0025"/>
      <value value="0.0050"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="advertising-effect">
      <value value="0"/>
      <value value="1.25E-4"/>
      <value value="2.5E-4"/>
      <value value="5.0E-4"/>
      <value value="0.0010"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Goldenberg_replication_short" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>percent16</metric>
    <metric>percent50</metric>
    <metric>percent95</metric>
    <enumeratedValueSet variable="size_of_weak_contacts">
      <value value="5"/>
      <value value="13"/>
      <value value="21"/>
      <value value="29"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="advertising-effect">
      <value value="5.0E-4"/>
      <value value="0.0080"/>
      <value value="0.011"/>
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strong-ties-effect">
      <value value="0.01"/>
      <value value="0.03"/>
      <value value="0.05"/>
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_personal_network">
      <value value="5"/>
      <value value="13"/>
      <value value="21"/>
      <value value="29"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weak-ties-effect">
      <value value="0.0050"/>
      <value value="0.0080"/>
      <value value="0.011"/>
      <value value="0.015"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Goldenberg_replication May 2012 a" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>percent16</metric>
    <metric>percent50</metric>
    <metric>percent95</metric>
    <enumeratedValueSet variable="size_of_weak_contacts">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="advertising-effect">
      <value value="0"/>
      <value value="2.5E-5"/>
      <value value="5.0E-5"/>
      <value value="1.0E-4"/>
      <value value="2.0E-4"/>
      <value value="4.0E-4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strong-ties-effect">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.04"/>
      <value value="0.08"/>
      <value value="0.16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_personal_network">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weak-ties-effect">
      <value value="5.0E-4"/>
      <value value="0.0010"/>
      <value value="0.0020"/>
      <value value="0.0040"/>
      <value value="0.0080"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Goldenberg_replication May 2012 4" repetitions="3" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>percent16</metric>
    <metric>percent50</metric>
    <metric>percent95</metric>
    <enumeratedValueSet variable="market-size">
      <value value="1024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_personal_network">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_weak_contacts">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strong-ties-effect">
      <value value="0.02"/>
      <value value="0.04"/>
      <value value="0.08"/>
      <value value="0.16"/>
      <value value="0.32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weak-ties-effect">
      <value value="0.0050"/>
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.04"/>
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="advertising-effect">
      <value value="1.0E-5"/>
      <value value="2.0E-5"/>
      <value value="4.0E-5"/>
      <value value="8.0E-5"/>
      <value value="1.6E-4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment 3 relative strength" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>percent16</metric>
    <metric>percent50</metric>
    <metric>percent95</metric>
    <enumeratedValueSet variable="Market2">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nStrong2">
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nWeak2">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AdEffect">
      <value value="-10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="WeakEffect">
      <value value="-9"/>
      <value value="-8"/>
      <value value="-7"/>
      <value value="-6"/>
      <value value="-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="StrongEffect">
      <value value="-5"/>
      <value value="-4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment 4 relative number" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>percent16</metric>
    <metric>percent50</metric>
    <metric>percent95</metric>
    <enumeratedValueSet variable="Market2">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nStrong2">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nWeak2">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="AdEffect">
      <value value="-9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="WeakEffect">
      <value value="-7"/>
      <value value="-6"/>
      <value value="-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="StrongEffect">
      <value value="-6"/>
      <value value="-5"/>
      <value value="-4"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
