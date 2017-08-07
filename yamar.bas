1 REM"...... .HE .AVES OF .AMAR
5 L=RND(-TI)
10 DIM DE$(51,5),RO$(50),VE$(46),OB$(71,2),LO(71),SP(21),PR$(5)
15 DIM CA$(14),DI$(9),LI$(2,2),LI(3),NO$(43),AT(21),VA(13),WD$(40)
20 GOSUB7000
30 GOSUB8000
100 PRINTLF$;
110 GOSUB5870
120 NU=RM
130 GOSUB9000
140 FORL=1TOLEN(VI$)
150 IF CH$=MID$(VI$,L,1)THEN184
160 NEXTL
165 VI$=VI$+CH$
170 GOSUB5851
171 GOTO185
184 GOSUB5857
185 PRINTLF$;
186 FORL=36TO59:IF(L>36ANDL<41)OR(L>51ANDL<59)THENLO(L)=RM
187 NEXTL
188 LO(44)=52:IFRM=15ORRM=5THENLO(44)=RM
190 OPEN1,0
194 PRINT">";:INPUT#1,TC$
198 CLOSE1:PRINT""
200 rem start of parser
201 cm$=""
202 iflen(tc$)=0thenprint"Excuse me?":goto185
203 iftc$="restart"then run
205 gosub5400
215 rem separate command into words
217 forl=1towo:wd$(l)="":next:wo=1:wf=1
220 forl=1tolen(cm$)
230 tc$=mid$(cm$,l,1)
240 ifwf=1andtc$=" "then280
250 wf=2
260 iftc$<>" "thenwd$(wo)=wd$(wo)+tc$:goto280
270 wf=1
275 ifwd$(wo)<>"the"andwd$(wo)<>"a"andwd$(wo)<>"an"thenwo=wo+1:goto280
276 wd$(wo)=""
280 nextl
290 wd$(wo+1)="":sn$="":ot$=""
295 vp=0:pp=0:l=1:of=1:op(1)=0:op(2)=0
310 forl1=1to46
320 ifwd$(l)=ve$(l1)thenvp=l1
330 nextl1
340 ifvp=0thenprint"You can't use '";wd$(1);"' as a verb.":goto 185
345 ifvp<20andwd$(l+1)=""then430
347 ifwd$(l+1)=""thengosub5110
350 l=l+1
355 ifl>wothen390
360 gosub5300
380 ifnu=0then ot$=ot$+sn$+wd$(l):sn$=" ":goto350
382 if(vp=19orvp=20orvp=22)andot$=""then410
385 ifot$=""thenprint"There's no direct object in that command":goto185
390 gosub5200
400 ifnu>1thengosub5100:goto390
401 ifop(of)=0thenprint"You can't use '";ot$;"' as a noun.":goto185
405 ifwd$(l+1)=""then415
410 of=of+1:ifof<3thenot$="":sn$="":goto347
415 co=0
416 forl=2to1step-1
420 iflo(op(l))=rm or lo(op(l))=0 or op(l)=0 or(lo(op(l))=60andrm=15)then4
25
421 co=l
425 nextl
426 ifco=0then430
427 print"There is no '";ob$(op(co),2)+" "+ob$(op(co),1);"' here.":goto185

430 no=0:di=0
431 ki=1:gosub5900:ifki=1thendi=3:goto442
433 ifvp<13thengosub500:goto6000
435 np=1
437 ifvp>27then455
440 onvp-12gosub600,600,610,610,700,710,750,780,810,850,880,880,910,930,95
0
442 ifdi>0then7500
443 printno$(no):ifno=2then185
450 goto6000
455 ifvp>40then470
460 onvp-27gosub960,1010,1050,1070,1090,1130,1150,1180,1200,1220,1240,1240
,1240
465 goto442
470 onvp-40gosub1240,1240,1300,1330,1365,1410
475 goto442
500 ifvp>6thenvp=vp-6
510 char$=mid$(room$(rm),vp,1)
520 gosub9050
530 ifnu>0thenrm=nu:goto560
540 ifsp(abs(nu))=1thench$=left$(ca$(abs(nu)),1):gosub9050:rm=nu:goto560
550 printright$(ca$(abs(nu)),len(ca$(abs(nu)))-1):np=1
560 return
600 rem look
601 no=4:gosub5850:return
610 rem inven
611 no=4:ifwgt=0thenprint"You aren't carrying anything.":return
620 print"You are carrying the following:"
630 forl=1to71
640 iflo(l)=0thengosub670:print
650 nextl
660 return
670 sp$=" ":ifob$(l,2)=""thensp$=""
671 tb=9:iflo(l)=60thentb=16
674 printtab(tb);"a ";
675 ifl<3thenprintob$(l,1);" (";li$(l,li(l));")";:goto690
680 printob$(l,2)+sp$+ob$(l,1);
690 return
700 no=4:gosub5800:return
710 gosub5800:print"Do you really want to quit?"
720 getr$:ifr$=""then720
730 ifr$<>"y"thenno=4:return
740 goto8550
750 rem jump
751 ifpp=0thenno=3:goto775
760 ifpp<2orpp>3thenno=2:goto775
765 ifop(2)=33thendi=2:goto775
770 no=1
775 return
780 rem turn
781 ifop(1)=45thenrm=17:no=6:return
785 ifpp<3orpp>4thenno=2:goto800
790 if(pp=3)and(op(1)=2orop(2)=2)andbd=0thenli(2)=1:goto800
795 ifpp=4and(op(1)=2orop(2)=2)thenli(2)=2:gosub801:goto800
796 no=1
800 return
801 ifsp(19)=0andrm=9thensp(19)=1:lo(61)=abs(lo(61)):goto803
802 return
803 print"The walls begin to glow and shake but   the symbol appears to be
 black."
804 print"Suddenly, the symbol falls off the wall and hits the ground."
805 return
810 rem read
820 ifop(1)<15orop(1)>23thenno=1:return
821 ifop(1)=23andsp(4)=0thenno=5:return
825 printlf$;
830 no=4:onop(1)-14gosub20000,20010,20020,20030,20040,20050,20060,20070,20
080
831 ifop(1)=17andlo(1)=60andlo(5)=60andli(1)=1then833
832 return
833 printlf$;"The table starts to tremble and the vaseshatters.";
834 print" On the table where the vase   used to be is a glistening diamon
d."
835 lo(5)=56:lo(63)=60:return
850 rem enter
860 ifop(1)=47thendi=4:return
865 ifpp=2andop(2)=47thendi=4:return
870 no=1
875 return
880 rem take
885 iflo(op(1))=0thenno=7:return
886 ifwg>5thenno=16:return
887 ifop(1)=13andmk=0thenlo(11)=abs(lo(11)):print"You find a metal key.":m
k=1
890 ifop(1)<19thenlo(op(1))=0:wg=wg+1:no=8:return
891 ct=0
895 ifop(1)>58thengosub5910:ifct=0thenlo(op(1))=0:wg=wg+1:no=8:goto900
896 ifct=0thenno=9:return
897 return
900 ifat(op(1)-58)=0thenat(op(1)-58)=1:sc=sc+va(op(1)-58)
905 return
910 rem wear
912 if op(1)=6andlo(6)=0thenwg=wg-1
915 ifop(1)=6thenlo(6)=53:return
917 ifop(1)=60or op(1)=70thenno=23:return
920 no=1
925 return
930 rem drop
935 iflo(op(1))<>0thenno=10:return
940 lo(op(1))=rm:no=11:wg=wg-1
945 return
950 rem push
955 ifop(1)=47thensp(2)=1-sp(2):return
956 no=12
957 return
960 rem open
965 ifop(1)=45then781
966 ifop(1)=36then996
969 ifop(1)<41orop(1)>44thenno=1:return
970 onop(1)-40goto975,980,990,1000
975 ifsp(6)=1thenno=15:return
976 ifsp(10)=0thenno=14:return
977 sp(6)=1:return
980 ifsp(5)=1thenno=15:return
981 ifsp(8)=0thenno=14:return
985 sp(5)=1:return
990 ifsp(1)=1thenno=15:return
991 iflo(14)=11thensp(1)=1:return
995 nu=1:no=4:goto550
996 ifsp(11)=1thenno=15:return
997 sp(11)=1:lo(9)=abs(lo(9)):no=17
998 return
1000 ifrm=5thenno=19:return
1005 print"You open the trap door and climb        through. ";
1007 rm=5:no=20:lo(64)=abs(lo(64))
1008 return
1010 rem close
1015 ifop(1)=36then1041
1018 ifop(1)<41orop(1)>44thenno=1:return
1020 onop(1)-40goto1025,1030,1035,1040
1025 ifsp(6)=0thenno=18:return
1027 sp(6)=0:return
1030 ifsp(5)=0thenno=18:return
1032 sp(5)=0:return
1035 ifsp(1)=0thenno=18:return
1037 sp(1)=0:return
1040 no=18:return
1041 ifsp(11)=0thenno=18:return
1042 sp(11)=0:return
1050 rem free
1051 ifop(1)<>30thenno=1:return
1055 ifsp(13)=1thenprint"Zomba's already freed.":return
1058 print"The man thanks you politely and rewards you with a scroll. He t
hen ";
1060 print"says ";q$;"Call mewhen you're ready,";q$;" and books out of the
room. ";
1063 print"As he left you noticed the word   'ZOMBA' written on his back."

1065 lo(17)=0:sp(13)=1:lo(30)=51:no=4:wg=wg+1:sc=sc+10:return
1070 rem play
1074 ifop(1)<>35thenno=1:return
1075 ifsp(4)=0thenno=21:return
1080 no=22:return
1090 rem light
1091 iflo(op(1))<>0thenno=10:return
1092 ifop(1)=2andop(2)=0thenpp=3:goto790
1093 ifop(2)=0thenno=24:return
1094 ifpp<>1thenno=2:return
1095 goto1115
1096 ifop(1)=1then1111
1100 ifop(1)>14andop(1)<19then1120
1105 no=1:return
1111 ifop(2)=7andli(3)=1thenli(1)=1:return
1113 goto1105
1115 ifop(2)=33thenno=26:lo(op(1))=51:wg=wg-1:return
1116 goto1096
1120 if(op(2)=7andli(3)=1)or(op(2)=1andli(1)=1)then1123
1121 goto1105
1123 no=25:lo(op(1))=51:wg=wg-1:return
1130 rem shake
1135 ifop(1)=31thenlo(16)=abs(lo(16)):no=37:return
1138 ifop(1)<19orop(1)>58thenno=12:return
1140 no=1:return
1150 rem say
1155 ifop(1)<52orop(1)>58thenno=27:return
1160 ifop(1)>51andop(1)<57then1170
1162 ifop(1)=57andsp(3)=0andrm=21thensp(3)=1:no=28:return
1165 ifop(1)=58andsp(13)=1thengosub3200:ifno=4thenreturn
1167 no=29:return
1170 iflo(25)<>rmthen1167
1175 tr=1:ts=mo:no=30:return
1180 rem rub
1185 ifop(1)=37andrm=20thenrm=7:lo(65)=abs(lo(65)):goto1195
1187 ifop(1)=33orop(1)=46thendi=5:return
1188 if((op(1)=7andop(2)=8)or(op(1)=8andop(2)=7))andpp=1then1190
1189 no=27:return
1190 print"A spark appears and the stick catches   fire. ";:li(3)=1:sf=mo
1191 print"The friction causes the stone to  crumble to dust."
1192 iflo(8)=0thenwg=wg-1
1193 lo(8)=56:no=4:return
1195 print"The wall spins around and you go with   it. You end up in a fam
iliar room.
1197 ifsp(15)=0thensp(15)=1:print"As this happened a small medal fell to  
the floor.
1198 no=4:return
1200 rem dig
1202 ifop(1)<>49thenno=1:return
1205 ifop(2)=0thenno=24:return
1207 ifpp<>1thenno=2:return
1208 ifop(2)<>12thenno=1:return
1210 ifsp(16)=0thenlo(66)=abs(lo(66)):sp(16)=1:no=31:return
1215 print"You find nothing.":no=4:return
1220 rem hit
1225 ifop(2)=0thenno=24:return
1227 ifpp<>1thenno=2:return
1230 ifop(1)>23andop(1)<31then 1240
1235 if op(1)=48andop(2)=7andsp(17)=0then1237
1236 no=12:return
1237 lo(15)=abs(lo(15)):sp(17)=1
1238 no=32:return
1240 rem combat
1241 ifop(2)=0thenno=24:return
1242 ifpp<>1thenno=2:return
1245 ifop(1)<24orop(1)>30thenno=1:return
1250 ifop(2)<>3andop(2)<>4andop(2)<>7andop(2)<>8andop(2)<>12thenno=12:retu
rn
1252 iflo(op(2))<>0thenno=10:return
1255 ifop(1)=27thenprint"Your weapon goes right thru the ghost.":no=4:retu
rn
1256 ifop(1)=24then1271
1258 ifop(1)=30thenlo(30)=56:print"The helpless prisoner is easily killed.
":no=33:return
1260 ifop(1)=25andtr=1then1270
1261 ifop(1)=25thenrn=2:goto1277
1265 ifop(1)=26then1271
1268 ifop(1)=28orop(2)=29thenrn=1:goto1277
1269 no=1:return
1270 print"The frozen Eddy is easily killed by your weapon.":no=33:sp(14)=
1:return
1271 rn=int(rnd(1)*4)+1
1272 ifol=rnthen1271
1273 ol=rn
1276 ifop(2)=8orop(2)=7thenrn=2
1277 no=4:ifrn=4thenlo(op(1))=56:no=33:gosub1290
1279 onrngoto1280,1282,1284,1286
1280 print"You just miss hitting your enemy.":return
1282 print"Your enemy knocks your weapon out of    your hands."
1283 lo(op(2))=rm:wg=wg-1:return
1284 print"Your attack is way off target.":return
1286 print"The enemy is caught off guard and you   slice him up.":sc=sc+10
:return
1290 ifop(1)=26thensp(18)=1:return
1295 sp(4)=1
1296 ph=1:return
1300 rem break
1310 ifop(2)=0thenno=24:return
1312 ifpp<>1thenno=2:return
1315 ifop(2)<>3andop(2)<>4andop(2)<>7andop(2)<>12thenno=1:return
1317 iflo(op(2))<>0thenno=10:return
1318 ifop(1)<>5andop(1)<>2thenno=38:return
1320 ifop(1)=5then1326
1322 iflo(2)=0thenwg=wg-1
1324 lo(2)=53:no=39:return
1326 iflo(5)=0thenwg=wg-1
1328 lo(5)=53:no=39:return
1330 rem unlock
1335 ifop(2)=0thenno=24:return
1337 iflo(op(2))<>0thenno=10:return
1340 ifop(1)<41orop(1)>43thenno=40:return
1345 onop(1)-40goto1350,1355,1360
1350 iflo(9)=0andop(2)=9andsp(10)=0thensp(10)=1:return
1351 no=1:return
1355 iflo(11)=0andop(2)=11andsp(8)=0thensp(8)=1:return
1356 no=1:return
1360 goto995
1365 rem put
1370 ifop(2)=0thenno=41:return
1375 ifpp<>2andpp<>3thenno=2:return
1376 iflo(op(1))<>0thenno=10:return
1380 onpp-1goto1385,1395
1385 ifop(2)=33thenwg=wg-1:lo(op(1))=56:no=26:return
1387 ifop(2)=51andop(1)=3thensp(7)=1:no=42:wg=wg-1:lo(3)=56:return
1389 no=1:return
1395 ifop(2)=34then1400
1397 ifop(2)=33then1385
1399 no=1:return
1400 iftw=3thenno=43:return
1405 tw=tw+1:lo(op(1))=60:wg=wg-1:return
1410 rem throw
1420 iflo(op(1))<>0thenno=10:return
1423 ifop(2)=0thenno=44:return
1426 ifpp=1orpp=4thenno=2:return
1430 ifop(2)=33then1385
1435 ifop(2)=27then1445
1437 print"Thrown."
1440 wg=wg-1:lo(op(1))=rm:no=4:return
1445 ifop(1)=10then1450
1446 print"The object goes right through the ghost.":goto1440
1450 print"The rusty key cuts and kills the ghost  as it passes through it
."
1451 lo(27)=56:sp(20)=1:sc=sc+10:goto1440
3000 rem enemy here?
3010 iflo(26)=rmthen3100
3020 iflo(27)=rmthen3120
3030 iflo(24)=rmthen3130
3035 iflo(30)=rmandsp(13)=0then3140
3040 return
3100 print"Facing you is an old man with glowing,  white, stringy hair. He
 is ";
3105 print"waving a     small bag that has something inside."
3110 return
3120 print"A white ghost carrying a long metal     chain is guarding the t
reasure."
3125 return
3130 print"Sitting at the organ and playing a"
3131 print"dirgeful requiem is a hideous phantom."
3132 print"Recognizing your presence, the phantom"
3133 print"gets up and brandishes a long cutlass."
3134 print"It appears he's not happy with your     intrusion."
3135 return
3140 print"On the east wall a helpless, bearded man";
3141 print"is chained up. His cries for help and"
3142 print"moans of agony echo throughout the room.";
3143 return
3200 ifrm<>1thenreturn
3202 wi=0
3205 forl=59to71
3210 iflo(l)<>rmthenwi=1
3215 nextl
3220 ifwi=1thenreturn
3225 ifsp(21)=1thenreturn
3230 sp(21)=1
3235 print"The bearded man appears and puts all    your treasures in a lar
ge sack."
3240 lo(13)=1:lo(30)=1
3241 forl=59to71:lo(l)=61:nextl
3245 no=4:return
4000 iflo(13)<>0thenprint"You can't leave the caves yet.":rm=1:goto6000
4010 printlf$;tab(12);"Congratulations!"
4020 print"You have defeated Yamar and escaped withall of his treaures."
4030 goto 8510
4100 ifde$="*"then4120
4115 printde$
4120 return
5000 open1,0
5020 input#1,r$:print
5030 close1
5040 return
5100 print"Which ";ot$;" do you mean? ":print">";:gosub5000:print
5101 ifan=1thenot$=r$+" "+ot$
5102 ifan=2thenot$=ot$+" "+r$
5103 return
5110 print"What do you want to ";cm$;"? ":print">";:gosub5000
5120 tc$=r$:gosub5400:ot$=cm$:return
5200 nu=0:op(of)=0:an=0
5205 forl1=1to71
5210 ifot$=ob$(l1,2)+" "+ob$(l1,1)thenop(of)=l1:goto5240
5213 ifot$=ob$(l1,1)thenop(of)=l1:nu=nu+1:an=1
5214 ifot$=ob$(l1,2)thenop(of)=l1:nu=nu+1:an=2
5230 nextl1
5240 return
5300 nu=0:forl1=1to5
5310 ifwd$(l)=pr$(l1)thenpp=l1:nu=1
5320 nextl1
5330 return
5400 cm$="":forl2=1tolen(tc$)
5405 n2=asc(mid$(tc$,l2,1))
5410 ifn2>159andn2<219thenn2=n2-128
5415 cm$=cm$+chr$(n2)
5420 nextl2
5425 return
5800 rem died score & moves
5801 xt$="time":iftd<>1thenxt$=xt$+"s"
5802 xm$="move":ifmo<>1thenxm$=xm$+"s"
5810 printlf$;"You have died";td;xt$;"."
5820 print"Your score is";sc;"in";mo;xm$;"."
5830 return
5850 gosub5870
5851 forl=1to5
5852 ki=1:gosub5902
5853 ifki=1thende$=de$(51,l):gosub4100:goto5855
5854 de$=de$(rm,l):gosub4100
5855 nextl
5856 ifki=1then5869
5857 forl=1to71:ifl=19thenl=59
5858 iflo(l)=rmthenprint"There is";:gosub670:print" here."
5859 iflo(l)=60andrm=15thenprint"On the table is";:gosub670:print"."
5863 nextl
5864 iflo(68)<>rmorsp(3)=1then5869
5865 print"The painting is of an old man standing"
5866 print"on a hill and a young boy falling to his";
5867 print"death, trying to fly with wings made of wax."
5869 gosub3000:return
5870 printright$(ro$(rm),len(ro$(rm))-6)
5875 return
5900 rem check if room is dark
5901 if(vp>12andvp<22)orvp=32thenki=0
5902 ifrm=2orrm=14orrm=23orrm=42thenki=0
5903 if(lo(1)=0orlo(1)=rmor(lo(1)=60andrm=15))andli(1)=1thenki=0
5904 if(lo(2)=0orlo(2)=rmor(lo(2)=60andrm=15))andli(2)=1thenki=0
5905 return
5910 rem could take?
5915 ct=0:ifop(1)=68andsp(3)=0thenct=1:no=34
5917 ifop(1)=69andsp(20)=0thenct=1:no=35
5919 ifop(1)=70andlo(6)<>53thenct=1:di=6
5921 ifop(1)=71andsp(18)=0thenct=1:no=36
5923 return
6000 rem inc and more
6005 ifph=1thenph=0:print"You notice something written on the     organ be
nch."
6010 mo=mo+1:ifsp(14)=1then6013
6011 iflo(25)<rm then lo(25)=lo(25)+1:goto6013
6012 iflo(25)<>rmthenlo(25)=lo(25)-1
6013 ifog=mo-2thendi=8:goto7500
6014 ifsr=mo-2thendi=9:goto7500
6015 ifrm=10andf1=0thenf1=1:og=mo
6016 ifrm=24andf2=0thenf2=1:sr=mo
6017 ifrm<>10andf1=1thenf1=0:og=-1
6018 ifrm<>24andf2=1thenf2=0:sr=-1
6020 ifrm=56thendi=1:goto7500
6025 ifrm=59then4000
6030 iflo(26)=rmorlo(27)=rmorlo(24)=rmthen6150
6031 iflo(25)=rmthen6220
6035 ifsf=mo-3andli(3)=1then6110
6037 ifts=mo-3andtr=1then6130
6100 si=rm:wn=wn-.5
6101 ifwn<0thenwn=0
6102 ifnp=1thennp=0:goto185
6105 goto100
6110 print"The hiking stick has burned up.":iflo(7)=0thenwg=wg-1
6120 lo(7)=56:goto6100
6130 tr=0
6135 ifrm<>lo(25)then6100
6140 print"Eddy has fallen out of the trance."
6145 goto6100
6150 rem enemy combat
6151 ifsi<>rmthense=0:goto 6100
6152 rs=5:ifsc<115thenrs=3
6155 rn=int(rnd(1)*rs+1)
6160 onrngosub6170,6180,6190,6200,6210
6161 ifwn>7thenwn=0:di=7:goto7500
6162 goto6100
6170 print"You dodge a fierce stab by the enemy."
6175 wn=wn-1:return
6180 print"Your enemy strikes you on your side."
6185 wn=wn+1:return
6190 print"The enemy hits and you stagger back."
6195 wn=wn+2:return
6200 print"The enemy swings and you suffer a       skinned arm."
6205 wn=wn+1:return
6210 print"The enemy attacks high but you duck the blow."
6215 wn=wn-2:return
6220 ifse=0thense=1:print"Eddy, the roaming corpse, is in the roomwith you
.":et=mo
6230 ifet=mo-6thense=0:lo(25)=42
6240 iftr=0then6150
6250 goto6100
7000 lf$="":q$=chr$(34)
7005 poke53281,15:poke53280,12
7010 print"..";tab(11);"The Caves of Yamar";lf$
7020 printtab(19);"by";lf$
7030 printtab(14);"John Dalton"
7040 printtab(17);".....";lf$
7070 print"You just moved into an old house"
7071 print"embedded within deep woods. One day"
7072 print"while hiking in the dreaded forest, you"
7073 print"noticed an old oak door leading into the";
7074 print"side of a mountain. Curious about what"
7075 print"the door revealed, you opened it and"
7076 print"stepped in. Suddenly, some ghostly"
7077 print"force slammed the door behind you and   you were left in darkne
ss."
7100 print"Do you want instructions (Y/N)?";
7110 getr$:ifr$<>"y"andr$<>"n"then7110
7120 ifr$="n"thenprint".":goto 7492
7130 print".";
7140 print"The object of the game is to find the 13";
7142 print"treasures of Yamar and escape with them"
7144 print"out of the cave. In the caves you may"
7146 print"encounter monsters which may or may not"
7148 print"have to be killed. To get out of the"
7150 print"cave, you must first put all the"
7160 print"treasures in the Entrance Room. After"
7162 print"doing this you are 'ready' to win."
7164 print"There is an 'alive' corpse that roams"
7165 print"around the caves with you."
7166 print"If you try to do something in the dark,"
7167 print"the corpse (Eddy) will sneak up on you  and probably kill you."

7170 gosub7495
7180 print".When you see the prompt, > , you then"
7185 print"type in your command. All commands must"
7187 print"have a verb. Some verbs include :"
7190 print"LOOK or L    - prints room description"
7192 print"INVEN or I   - prints what objects you     have"
7194 print"SCORE        - prints # of times died,     your score and # of 
moves"
7195 print"   note: score is obtained from getting treasures or killing mo
nsters"
7198 print"QUIT            - ends game"
7200 gosub7495
7210 print".To move around type a compass"
7220 print"direction (n,s,e,w) or u(up) or d(down)."
7225 print"You may also type out the whole word."
7230 print"When you need to type an object you can"
7240 print"type the adjective or the noun unless"
7242 print"there is more than one. (ex. there is   more than one sign.)"
7244 print"Examples of commands are:"
7246 print"    put iron key on the table"
7247 print"    turn on the flashlight"
7248 gosub7495
7250 print".   The maximum score possible is 300.               Good Luck!
"
7492 return
7495 print"   press SPACE to continue"
7496 getr$:ifr$<>" "then7496
7497 return
7500 printlf$;di$(di)
7510 print"";tab(7);">>>>> YOU HAVE DIED <<<<<"
7520 td=td+1:sc=sc-5
7530 iftd=3then8500
7536 forl=59to71:iflo(l)=0thenlo(l)=42:wg=wg-1
7537 nextl
7540 forl=1to18:iflo(l)=0thenlo(l)=int(rnd(1)*9+26):wg=wg-1
7545 nextl
7549 rm=11:lo(2)=11:li(2)=1:forde=1to750:next
7550 print"  You have just woken up from a horrible";
7560 print"'dream.' Your body aches but you seem to";
7570 print"be alive."
7580 np=0:goto100
8000 print".   loading data, please wait..."
8005 forl=1to50:readro$(l):next
8010 forl=1to46:readve$(l):next
8020 forl=1to5:readpr$(l):next
8025 forl=1to71:readob$(l,1),ob$(l,2):next
8030 forl=0to71:readlo(l):next
8035 forl=0to14:readca$(l):next
8040 forl=1to9:readdi$(l):next
8045 forl=1to2:read li$(l,1),li$(l,2):next
8050 forl=0to43:readno$(l):next
8055 forl=1to13:readva(l):next
8070 rm=1:vi$="":wgt=3:td=0:ph=0:tw=1:sf=0:tr=1:ts=0:wn=0:se=0:sh=0:sy=0:s
g=0
8077 og=9999:sr=9999:mk=0
8080 forl=0to21:sp(l)=0:at(l)=0:next:sp(9)=1
8090 li(1)=2:li(3)=2:li(2)=2
8100 forl=1to51:forl1=1to5
8101 ifl<27or(l>34andl<47)orl=51thenreadde$(l,l1):goto8103
8102 de$(l,l1)=de$(26,l1)
8103 nextl1,l
8112 return
8150 print"."
8500 rem end of game
8505 print"";tab(12);"The Game is Over"
8507 printlf$;
8510 gosub 5820
8520 print"Do you want to explore the caves again?"
8530 getr$:ifr$=""then8530
8540 ifr$="y"thenrun
8550 print"";tab(6);"---END OF SESSION---":goto63999
9000 rem convert # to $
9010 ch$=chr$(nu+64)
9020 return
9050 rem convert $ to #
9060 nu=asc(ch$)
9065 ifnu>191thennu=nu-96
9070 nu=nu-64
9075 return
12000 rem  rooms and room pointers
12001 data"b@@;@@Entrance Room","ca@@4@Acacia Room"
12010 data"@b@@@dTransylvania Room","f@e@c@Prison Room"
12020 data"@@@d@6Small Catacomb","@dgN@@Dark Passage"
12030 data"h@@f@@Cold Tunnel","kgji@@The Morgue"
12040 data"@@hl@@Room of Gold Walls","@@mh@nGangland"
12050 data"?h@@@@Purgatory","7@i@@7South of Black Abyss"
12060 data"@@@jL@Gallows Room","@op@j@Fire Room"
12070 data"n@@@3@Beast Room","@<@nt@Opera Room"
12080 data"p@r@@@Room of Poison Dust","sssq@@Pentagon Room"
12090 data"@@@@8@Dark Corridor","@@@u@@Cave of Cold Walls"
12100 data"=ytx@@Long Hallway","@u>@@@Iron Maiden Room"
12110 data"@@@v@@Sanctuary","@@2@@aSpider Room"
12120 data"u@z@@@Warm Cave","^@[y@@Small Corridor"
12130 data"@[zy\@Small Corridor","[y@@@]Small Corridor"
12140 data"@@R[@@Small Corridor","�@@_�@Small Corridor"
12150 data"^@@^@ASmall Corridor","@B@z@@Small Corridor"
12160 data"@__y@OSmall Corridor","@@�^D@Small Corridor"
12170 data"@:@A@@Smoke-filled Room","E@@@@FHot Cave"
12180 data"@Dy@@7Lava Room","GH@@@@Arrakis Room"
12190 data"@F@@@7Pool of Dark Depths","F9@@D@Nile Room"
12200 data"H777@@Strange World","C@@@@@Yamar Room"
12210 data"@@L@@@Ghost Room","@@@K@mGhostly Tunnel"
12220 data"@k@@@@Lost Tomb","@@f@@@Clock Room"
12230 data"yAyPA@Small Corridor","@_[AQ@Small Corridor"
12240 data"qyQO@CSmall Corridor","\R@]@@Small Corridor"
12249 :
12500 rem   verbs
12501 datan,s,e,w,u,d,north,south,east,west,up,down
12502 datalook,l,inven,i,score,quit
12505 datajump,turn
12507 dataread,enter,get,take,wear,drop,push,open,close,free,play
12508 datalight,shake,say
12510 datarub,dig,hit,whip,stab,kill,fight
12520 datacut,break,unlock,put,throw
12530 :
12540 rem  prepositions
12600 datawith,in,on,off,at
12610 :
12620 rem  nouns
12700 datatorch,&,flashlight,&,pitchfork,five-pronged,jackknife,,vase,blac
k
12710 datagloves,pair of,stick,hiking,stone,strange
12720 datakey,iron,key,rusty,key,metal,spade,,sack,,cadaver,mutilated
12730 databook,brown,note,folded,scroll,,paper,piece of
12740 dataplaque,metallic,sign,square,sign,small,sign,wooden,bench,organ
12750 dataphantom,,eddy,,yamar,,ghost,,ogre,,spider,,prisoner,""
12760 datatree,acacia,clock,,fire,,table,,organ,,mouth,""
12770 datawall,north,wall,south,wall,east,wall,west
12780 datadoor,iron,door,oak,door,steel,door,trap,handle,,dust,poison
12790 datacasket,iron maiden,bookcase,,sand,,lava,molten,holes,five
12795 databd,,dm,,as,,sh,,nm,,icarus,,zomba,""
12800 dataruby,fulgent,ring,emerald,coin,gold eagle,cross,silver
12810 datadiamond,glistening,dish,china,medal,bronze,nugget,gold
12820 dataingot,platinum,painting,rare,goblet,silver,necklace,pearl
12830 datagems,bag of
12840 :
12850 rem locations of objects
12900 data-9,14,0,15,0,60,34,0,41,-13,35,-1,27,-1,8,-23,-2,-4,8,14,13,25,3
8
12910 data16,16,42,42,43,10,24,4,2,46,14,15,16,13,51,51,51,51,35,1,11,52
12920 data19,17,22,23,38,37,40,51,51,51,51,51,51,51,22,37,-9,23,-15,-5,-7
12930 data-38,45,21,43,17,42
12940 :
13000 rem can't go strings
13005 data" You can't go that way."
13010 data"MThe door is supernatural and it won't   open."
13020 data"wThere is no visible opening."
13030 data"vYou can't walk through walls."
13040 data"qThe phantom won't let you by."
13050 data"�There is an oak door there."
13060 data"JAn iron door blocks your way."
13070 data"IThe holes are too small to go through."
13080 data"sYou bang your head on the handle."
13090 data"X"
13100 data"eThe trap door is closed."
13110 data" "
13120 data"bThere's no way you can reach the hole."
13130 data"eYou'll have to open the trap door first."
13140 data"xI don't think the spiders will let you  by."
13200 :
13500 rem   die strings
13501 data"You have fallen into a flaming inferno."
13510 data"You hop into the flames and gradually   burn to a crisp."
13520 data"Eddy comes out of nowhere and saws your head off."
13525 data"The casket quickly closes and each spikestabs your body."
13530 data"Rubbing that is definitely fatal."
13540 data"The dust on the pearl necklace instantlypoisons you."
13550 data"Your body couldn't take that last hit   and you fall to the gro
und."
13555 data"The unfriendly ogres have ripped your   face off."
13556 data"The spiders have climbed up your leg andabout ten have bitten y
ou."
13600 rem  light strings
13601 data"lighted","burned-out","on","off"
13602 :
13700 rem    no strings
13701 data"Done.","You can't do that."
13710 data"I don't understand your command."
13720 data"You look like a fool.",""
13725 data"The phantom is in your way."
13726 data"An opening appears and you climb        through."
13730 data"You already have it.","Taken.","You can't take that."
13734 data"You don't have it.","Dropped."
13735 data"That isn't very helpful."
13740 data"You can't open that."
13741 data"The door is locked."
13745 data"It is already open.","Your hands are full."
13750 data"You hear something metal fall on the    ground."
13751 data"It is already closed."
13752 data"An invisible force among the bones      prohibits that."
13753 data"As the door snapped shut, some of the bones shifted positions."

13755 data"The phantom won't let you near it."
13760 data"A stentorian sound fills the air."
13763 data"It doesn't fit."
13765 data"You have to tell me what you want to do it with."
13767 data"It catches fire and disappears with the smoke."
13768 data"The flames of the fire instantly crematethe object."
13770 data"Nothing happens."
13775 data"The picture falls from the wall reveal- ing a small door to the
 north."
13778 data"Nothing happens here."
13780 data"Eddy falls into a trance as if he knew  what those initials sto
od for.
13782 data"Your spade digs up a shiny gold nugget."
13783 data"The bookcase shakes and a book falls    from one of the shelves
."
13784 data"The fresh carcass starts to disintegrateand soon disappears."
13785 data"The painting is securely fastened to the wall."
13786 data"The ghost protects the silver goblet."
13787 data"You can't take the bag from Yamar"
13788 data"Something white flutters to the ground."
13790 data"You can't break that."
13791 data"The object shatters into a 256 pieces."
13792 data"It isn't locked."
13793 data"You have to tell me where you want to   put it."
13794 data"The pitchfork becomes red hot and       explodes. The holes ope
n widely.
13795 data"There's no room."
13799 :
13800 rem point values
13801 data25,20,20,15,25,25,15,25,25,10,20,15,20
13899 :
19999 rem    read routines
20000 printq$;"...the sparks that made the fire,"
20001 print"were made by rubbing stick and stone.";q$
20002 printlf$;q$;"...gold will be found where Eagles     Dare.";q$
20003 printlf$;q$;"...the Eyes of the Nile are opening -- you'll see.";q$
20009 return
20010 printq$;"Torches blazed and sacred chants were  phrased.";q$
20011 return
20020 printq$;"O God of Earth and Altar,"
20021 print"Bow down and hear our cry,"
20022 print"Our earthly rulers falter,"
20023 print"Our people drift and die,"
20024 print"The walls of gold entomb us,"
20025 print"The swords of scorn divide,"
20026 print"Take not thy thunder from us,"
20027 print"But take away our pride.";q$
20029 return
20030 printq$;"...the Secret of the Hanged Man -- the smile on his lips...
";q$
20031 return
20040 printtab(11);q$;"Jump in the fire!";q$
20041 return
20050 print"The sign describes an innocent exile."
20051 return
20060 print"two: D M S H M"
20061 return
20070 print"  Welcome to Arrakis, a land that's richin spice."
20072 print"";tab(14);"Your leader,"
20073 printtab(14);"the Kwizatz Haderach"
20074 print"";tab(9);"property of Gom Yamar"
20075 return
20080 print"one: B D A S N"
20081 return
22221 rem".                   
22222 REM".                            .....
22223 REM".                   
40000 DATA".HIS IS A LARGE ROOM WITH A DAMP"
40001 DATA"GROUND. .HERE IS A SMALL EXIT LEADING"
40002 DATA"OUT OF THE NORTH END OF THE ROOM."
40003 DATA".PIDER WEBS ARE HANGING FROM THE HIGH"
40004 DATA"CEILING NEAR THE EXIT."
40005 DATA".HIS IS A SMALL AREA WITH AN OPENING IN"
40006 DATA"THE CEILING. .IGHT SHINES FROM THE HOLE"
40007 DATA"ONTO A SHORT ACACIA TREE STANDING IN"
40008 DATA"THE CORNER. .RITTEN ON THE EAST WALL IS"
40009 DATA"NUMBER TWENTY-TWO. .OU CAN EXIT TO THE  NORTH OR SOUTH."
40010 DATA".HIS IS AN EERIE ROOM WITH A LOW"
40011 DATA"CEILING. .YSTERIOUS TABLEAUX OF HIDEOUS"
40012 DATA"CREATURES FILL THE BLACK WALLS. .NE"
40013 DATA"PARTICULAR TABLEAU PORTRAYS AN OLD      CROSS-EYED WOMAN HOLDIN
G A TORCH"
40014 DATA"IN ONE HAND AND A LONG SCROLL IN THE    OTHER. . LADDER LEADS D
OWNWARD."
40015 DATA". COOL MIST OCCUPIES THIS STONE-WALLED"
40016 DATA"CHAMBER. .RY BLOOD STAINS THE SOUTH"
40017 DATA"WALL WHERE MANY CHAINS ARE ATTACHED."
40018 DATA". LADDER LEADS UP AND OTHER ESCAPE"
40019 DATA"ROUTES ARE NORTH AND EAST."
40020 DATA".ONES OF UNFORTUNATE PRISONERS ARE"
40021 DATA"SCATTERED ACROSS THE FLOOR OF THIS"
40022 DATA"DREADED ROOM. . SMALL TRAP DOOR IS"
40023 DATA"LOCATED BENEATH THE WITHERED BONES. .OU"
40024 DATA"NOTICE THAT IT'S DIFFICULT TO BREATHE   IN THIS ENCLOSED AREA."

40025 DATA".HIS IS A SMALL DARK PASSAGE WITH EXITS"
40026 DATA"LEADING SOUTH WEST AND EAST. .UMPS ON"
40027 DATA"THE GROUND CAUSE YOU TO TRIP AND"
40028 DATA"STUMBLE."
40029 DATA"*"
40030 DATA".HIS LONG ROOM IS UNUSUALLY COLD. .OUR"
40031 DATA"HEAD BARELY MISSES SCRAPING THE LOW"
40032 DATA"CEILING AS YOU PASS THROUGH. .HE SOUTH"
40033 DATA"WALL IS ESPECIALLY COLD AND YOUR HAND"
40034 DATA"NEARLY TURNS NUMB AS YOU TOUCH IT."
40035 DATA".HIS IS A MORBID PLACE WITH BLACK"
40036 DATA"WALLS. .HERE IS AN EXIT LEADING OUT OF"
40037 DATA"EACH OF THE FOUR WALLS."
40038 DATA"*"
40039 DATA"*"
40040 DATA".OU ARE STUNNED AS YOU ENTER THIS"
40041 DATA"SPARKLING ROOM OF SOLID GOLD WALLS."
40042 DATA".NGRAVED ON THE NORTH WALL IS A WEIRD-"
40043 DATA"LOOKING SYMBOL. .T CONSISTS OF A LARGE  CIRCLE TWO SMALLER ONES
 AND"
40044 DATA"AN ARROW POINTING DOWN. .XITS LEAD EAST AND WEST."
40045 DATA".HIS IS A LARGE TERRITORY OCCUPIED BY A"
40046 DATA"GANG OF OGRES WHO STARTED TO FOAM AT"
40047 DATA"THE MOUTH WHEN YOU ENTERED. .NE OF THE"
40048 DATA"OGRES IS HOLDING A RAT IN A TRAP IN     WHICH HE IS FEASTING ON
. .HERE IS"
40049 DATA"A PASSAGE TO THE EAST AND A HOLE IN THE GROUND."
40050 DATA".UNDREDS OF LOST SOULS CAN BE HEARD"
40051 DATA"CRYING OUT IN PAIN IN THIS PLACE. .FTER"
40052 DATA"A FEW MINUTES IN THIS ROOM YOUR THROAT"
40053 DATA"BECOMES DRY AND YOUR BODY BECOMES NUMB."
40054 DATA".O THE NORTH IS A STEEL DOOR."
40055 DATA".O THE NORTH OF THIS ROOM IS A HUGE PIT"
40056 DATA"LEADING DOWNWARD INTO THE EARTH. .NGRY"
40057 DATA"FLAMES ARE RISING FROM THE PIT AND"
40058 DATA"SCORCHING THE WALLS. .SSENCE OF BURNING"
40059 DATA"FLESH CAN BE SMELLED HERE. .OICES CAN BEHEARD CALLING YOU TO EN
TER."
40060 DATA".HE FIRST THING YOU NOTICE IN THIS ROOM"
40061 DATA"IS A MAN HANGING FROM THE CEILING AND"
40062 DATA"BLEEDING AT THE NECK. .N THE GROUND IS"
40063 DATA"A SQUARE SIGN. .T THE EASTERN END OF THEROOM THERE IS AN OLD WO
ODEN"
40064 DATA"STAIRCASE WINDING UPWARD AND THERE IS ANOPENING LEADING WEST."
40065 DATA".HERE IS A MONSTROUS FIRE LOCATED IN"
40066 DATA"MIDDLE OF THIS ROOM OF INTENSE HEAT."
40067 DATA".ASSAGES LEAD SOUTH EAST AND UP."
40068 DATA".ANGING ON THE WEST WALL IS A METALLIC"
40069 DATA"PLAQUE."
40070 DATA".HIS IS A SMALL DARK ROOM WITH BLOODY-"
40071 DATA"RED WALLS. . SMALL TABLE WITH BURN"
40072 DATA"MARKS IS LOCATED IN THE CENTRE OF THIS"
40073 DATA"ROOM. .OT FAR ABOVE YOUR HEAD IS A"
40074 DATA"HINGED TRAP DOOR."
40075 DATA".N THE SOUTHWESTERN CORNER OF THIS ROOM"
40076 DATA"THERE IS A LARGE ORGAN. .BOVE THE ORGAN"
40077 DATA"ON THE CEILING IS A SMALL OPENING.","*","*"
40078 DATA".HIS IS A FAIRLY LARGE ROOM BLANKETED"
40079 DATA"WITH A FINE LAYER OF POISONOUS GREY"
40080 DATA"DUST. .HERE ARE DOORWAYS TO THE NORTH"
40081 DATA"AND EAST."
40082 DATA"*"
40083 DATA".OU ARE IN A ROOM WITH FIVE WALLS WHICH"
40084 DATA"SEEM TO BE SPINNING AROUND YOU. .HERE"
40085 DATA"SEEM TO BE FOUR EXITS BUT YOUR NOT SURE"
40086 DATA"BECAUSE THIS ROOM IS MAKING YOU VERY"
40087 DATA"DIZZY."
40088 DATA".HIS IS A SMALL PASSAGE WITH SEEMINGLY"
40089 DATA"NO EXITS. .OWEVER A HANDLE OF SOME"
40090 DATA"SORT IS ATTACHED TO THE LOW CEILING."
40091 DATA"*"
40092 DATA"*"
40093 DATA".OU QUICKLY BEGIN TO SHIVER AS YOU"
40094 DATA"ENTER THIS CAVE. .OUCHING ANY OF THE"
40095 DATA"WALLS WOULD PROBABLY NUMB YOUR HANDS."
40096 DATA"*"
40097 DATA"*"
40098 DATA".HIS IS A LONG NARROW CORRIDOR WITH"
40099 DATA"EXITS TO THE EAST SOUTH AND WEST."
40100 DATA"*"
40101 DATA"*"
40102 DATA"*"
40103 DATA".HIS IS A LARGE ROOM FILLED WITH"
40104 DATA"PAINTINGS TOO HIGH TO REACH. .TANDING"
40105 DATA"AGAINST THE EAST WALL IS A LARGE METAL"
40106 DATA"CASKET IN THE SHAPE OF A WOMAN FILLED   WITH SPIKES. .HE .RON .
AIDEN IS"
40107 DATA"OPEN AND THE EYES OF THE WOMAN'S FACE   INVITE YOU TO ENTER IT.
"
40108 DATA".HIS ROOM IS A SACRED PLACE WITH A"
40109 DATA"BLINDING LIGHT COMING FROM THE NON-"
40110 DATA"VISIBLE CEILING. .TTACHED TO THE NORTH"
40111 DATA"WALL IS A LARGE BOOKCASE. . GHOSTLY"
40112 DATA"FORCE PROHIBITS REMOVING ANY OF THE     BOOKS FROM THE SHELVES.
"
40113 DATA".S YOU STEP INTO THIS ROOM A HUGE"
40114 DATA"GROUP OF TARANTULAS SWARM INTO THE ROOM"
40115 DATA"AND BLOCK THE EAST EXIT. .OWEVER THEY"
40116 DATA"SEEM TO HAVE LEFT YOU AN ESCAPE ROUTE   BECAUSE IN THE CORNER O
F THE ROOM"
40117 DATA"THERE IS A PASSAGE COVERED WITH WEBS    LEADING DOWN."
40118 DATA".HIS CAVE HAS STONE WALLS WHICH ARE"
40119 DATA"WARM TO THE TOUCH. . SMALL EXIT LEADS"
40120 DATA"EAST WHILE A LARGER ONE EXITS TO THE"
40121 DATA"NORTH. .HERE IS A SMALL SIGN ON THE"
40122 DATA"WEST WALL."
40123 DATA".OU ARE IN A SMALL WINDING MAZE-LIKE"
40124 DATA"CORRIDOR. .OU ARE NOT SURE OF ANY"
40125 DATA"EXITS."
40126 DATA"*"
40127 DATA"*"
40128 DATA".N THIS ROOM YOU ARE SURROUNDED BY"
40129 DATA"CLOUDS OF ACRID SMOKE. .O THE SOUTH IS"
40130 DATA"A LARGE IRON DOOR. .EYOND THE DOOR YOU"
40131 DATA"CAN HEAR SOME RATTLING NOISES."
40132 DATA"*"
40133 DATA".OU ARE IN A ROOM WHOSE NORTH WALL IS"
40134 DATA"UNBEARABLY HOT. . HAZE OF HOT STEAM IS"
40135 DATA"POURING IN FROM A SMALL DOOR TO THE"
40136 DATA"NORTH. .N THE NORTHWEST CORNER OF THE"
40137 DATA"CAVE A WINDING STAIRCASE COVERED WITH   SAND LEADS DOWN."
40138 DATA".HE HEAT IS EVEN MORE UNBEARABLE IN"
40139 DATA"HERE. .HE GROUND IS MADE UP OF COOLED"
40140 DATA"LAVA ROCK BUT IN THE CENTRE OF THE ROOM"
40141 DATA"THERE IS A CAMOUFLAGED DOOR IS SLIGHTLY"
40142 DATA"OPEN."
40143 DATA".HIS IS A LARGE ROOM FILLED WITH DRY"
40144 DATA"HOT SAND. .OUR FEET SINK ABOUT ONE INCH"
40145 DATA"INTO THE DUNE. .AILED TO THE WEST WALL"
40146 DATA"IS A WOODEN SIGN. .XITS LEAD NORTH AND"
40147 DATA"SOUTH."
40148 DATA".HIS IS A RECTANGULAR ROOM WITH JET"
40149 DATA"BLACK WALLS. .N THE MIDDLE OF THE ROOM"
40150 DATA"IS A DARK CREVICE WHOSE BOTTOM CANNOT"
40151 DATA"BE SEEN. .N THE CEILING THERE ARE"
40152 DATA"IMAGES OF FACES BECKONING YOU. .OMETHINGSHINY GLITTERS FROM THE
 POOL."
40153 DATA".HIS IS A WIDE ROOM WITH A STAIRCASE"
40154 DATA"LEADING UPWARD. .ROSSING THE MIDDLE OF"
40155 DATA"THE ROOM IS A SMALL RILL WHICH LOOKS"
40156 DATA"LIKE AN EXACT REPLICA OF THE .ILE"
40157 DATA".IVER. .N THE SOUTH WALL THERE ARE FIVE SMALL LINED-UP HOLES."
40158 DATA".HIS A LARGE AREA WITH SMALL FERNS"
40159 DATA"GROWING OUT OF THE CEILING. .O THE"
40160 DATA"SOUTH EAST AND WEST ARE LARGE"
40161 DATA"OPENINGS. . BLINDING LIGHT SHINES"
40162 DATA"THROUGH EACH PASSAGE."
40163 DATA".HIS IS A BIG ROOM LIGHTED BY NUMEROUS"
40164 DATA"BLACK CANDLES. .OUD MUSIC IS PLAYING"
40165 DATA"HERE."
40166 DATA"*"
40167 DATA"*"
40168 DATA".HIS IS A ROOM WITH MANY SMALL"
40169 DATA"TOMBSTONES ON THE GROUND. .OST OF THE"
40170 DATA"STONES READ  ......  BUT SOME SAY"
40171 DATA"'......' ."
40172 DATA"*"
40173 DATA".HIS IS A LONG TUNNEL WITH WHITE WALLS."
40174 DATA".HERE IS A LARGE HOLE LEADING DOWN AND"
40175 DATA"AN OPEN DOOR TO THE WEST."
40176 DATA"*"
40177 DATA"*"
40178 DATA".HIS IS A DESERTED SEPULCHER WITH A FEW"
40179 DATA"BONES AND STONES SCATTERED ABOUT. .HERE"
40180 DATA"IS A VERY DISTASTEFUL ODOR LINGERING IN"
40181 DATA"THE AIR."
40182 DATA"*"
40183 DATA".OU ARE IN A SMALL ROOM WITH ONE EXIT."
40184 DATA".GAINST THE NORTH WALL IS AN OLD"
40185 DATA".RANDFATHER CLOCK SET AT 13 MINUTES"
40186 DATA"PAST 10 O'CLOCK. .HE CLOCK APPARENTLY   ISN'T WORKING."
40187 DATA"*"
40188 DATA".HE ROOM IS IN TOTAL DARKNESS."
40189 DATA".OU CANNOT SEE A THING."
40190 DATA"*","*","*"
63999 END
