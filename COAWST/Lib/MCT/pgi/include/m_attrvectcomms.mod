V26 m_attrvectcomms
19 m_AttrVectComms.F90 S582 0
03/26/2019  11:41:29
use m_globalmap private
use m_globalsegmap private
use m_attrvect private
use m_list private
use m_string private
use m_globalmap private
use m_globalsegmap private
use m_attrvect private
use m_list private
use m_string private
enduse
D 194 24 971 624 970 7
D 289 18 115
D 291 24 1309 192 1308 7
D 322 24 1309 192 1308 7
D 328 24 1413 280 1410 7
D 440 24 1413 280 1410 7
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 29 0 0 0 0 0 0 m_attrvectcomms
R 600 19 8 m_string tochar
R 601 19 9 m_string string_init
R 602 19 10 m_string init
R 603 19 11 m_string string_clean
R 604 19 12 m_string clean
R 605 19 13 m_string string_len
R 606 19 14 m_string string_bcast
R 607 19 15 m_string bcast
R 608 19 16 m_string string_mci
R 609 19 17 m_string string_mco
R 610 19 18 m_string ptr_chars
R 611 19 19 m_string char
R 619 19 27 m_string len
S 757 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 774 19 15 m_list init
R 775 19 16 m_list clean
R 776 19 17 m_list nullify
R 777 19 18 m_list get_indices
R 778 19 19 m_list test_indices
R 779 19 20 m_list nitem
R 780 19 21 m_list get
R 781 19 22 m_list identical
R 783 19 24 m_list copy
R 784 19 25 m_list exporttochar
R 785 19 26 m_list exporttostring
R 786 19 27 m_list charbuffersize
R 787 19 28 m_list append
R 788 19 29 m_list concatenate
R 789 19 30 m_list bcast
R 790 19 31 m_list send
R 791 19 32 m_list recv
R 792 19 33 m_list getsharedlistindices
R 798 19 39 m_list index
R 809 19 50 m_list allocated
R 970 25 5 m_attrvect attrvect
R 971 5 6 m_attrvect ilist attrvect
R 972 5 7 m_attrvect rlist attrvect
R 975 5 10 m_attrvect iattr attrvect
R 976 5 11 m_attrvect iattr$sd attrvect
R 977 5 12 m_attrvect iattr$p attrvect
R 978 5 13 m_attrvect iattr$o attrvect
R 982 5 17 m_attrvect rattr attrvect
R 983 5 18 m_attrvect rattr$sd attrvect
R 984 5 19 m_attrvect rattr$p attrvect
R 985 5 20 m_attrvect rattr$o attrvect
R 987 19 22 m_attrvect init
R 988 19 23 m_attrvect clean
R 989 19 24 m_attrvect zero
R 990 19 25 m_attrvect lsize
R 991 19 26 m_attrvect niattr
R 992 19 27 m_attrvect nrattr
R 993 19 28 m_attrvect indexia
R 994 19 29 m_attrvect indexra
R 995 19 30 m_attrvect getilist
R 996 19 31 m_attrvect getrlist
R 997 19 32 m_attrvect exportilist
R 998 19 33 m_attrvect exportrlist
R 999 19 34 m_attrvect exportilisttochar
R 1000 19 35 m_attrvect exportrlisttochar
R 1001 19 36 m_attrvect appendiattr
R 1002 19 37 m_attrvect appendrattr
R 1003 19 38 m_attrvect exportiattr
R 1004 19 39 m_attrvect exportrattr
R 1005 19 40 m_attrvect importiattr
R 1006 19 41 m_attrvect importrattr
R 1007 19 42 m_attrvect copy
R 1008 19 43 m_attrvect rcopy
R 1009 19 44 m_attrvect icopy
R 1010 19 45 m_attrvect sort
R 1011 19 46 m_attrvect permute
R 1012 19 47 m_attrvect unpermute
R 1013 19 48 m_attrvect sortpermute
R 1014 19 49 m_attrvect sharedattrindexlist
S 1277 19 0 0 0 8 1 582 7180 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 174 2 0 0 0 0 0 582 0 0 0 0 gather
O 1277 2 1283 1282
S 1278 19 0 0 0 8 1 582 7187 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 178 2 0 0 0 0 0 582 0 0 0 0 scatter
O 1278 2 1285 1284
S 1279 19 0 0 0 8 1 582 4818 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 180 1 0 0 0 0 0 582 0 0 0 0 bcast
O 1279 1 1286
S 1280 19 0 0 0 8 1 582 5468 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 182 1 0 0 0 0 0 582 0 0 0 0 send
O 1280 1 1287
S 1281 19 0 0 0 8 1 582 5473 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 184 1 0 0 0 0 0 582 0 0 0 0 recv
O 1281 1 1288
S 1282 27 0 0 0 8 1400 582 7195 10010 400000 A 0 0 0 0 0 0 205 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gm_gather_
Q 1282 1277 0
S 1283 27 0 0 0 8 1699 582 7206 10010 400000 A 0 0 0 0 0 0 258 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gsm_gather_
Q 1283 1277 0
S 1284 27 0 0 0 8 1709 582 7218 10010 400000 A 0 0 0 0 0 0 259 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gm_scatter_
Q 1284 1278 0
S 1285 27 0 0 0 8 1717 582 7230 10010 400000 A 0 0 0 0 0 0 260 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gsm_scatter_
Q 1285 1278 0
S 1286 27 0 0 0 8 1725 582 4908 10010 400000 A 0 0 0 0 0 0 261 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 bcast_
Q 1286 1279 0
S 1287 27 0 0 0 8 1291 582 5702 10010 400000 A 0 0 0 0 0 0 185 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 send_
Q 1287 1280 0
S 1288 27 0 0 0 8 1298 582 5708 10010 400000 A 0 0 0 0 0 0 186 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 recv_
Q 1288 1281 0
S 1289 3 0 0 0 289 0 1 0 0 0 A 0 0 0 0 0 0 0 0 7243 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 20 4d 43 54 3a 3a 6d 5f 41 74 74 72 56 65 63 74 43 6f 6d 6d 73
S 1290 16 0 0 0 289 1 582 4974 14 440000 A 0 0 0 0 0 0 0 0 1289 306 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 1291 23 5 0 0 0 1297 582 5702 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 send_
S 1292 1 3 1 0 194 1 1291 7264 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav
S 1293 1 3 1 0 6 1 1291 5901 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 dest
S 1294 1 3 1 0 6 1 1291 5906 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tagbase
S 1295 1 3 1 0 6 1 1291 5085 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1296 1 3 2 0 6 1 1291 5894 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 status
S 1297 14 5 0 0 0 1 1291 5702 10 400000 A 0 0 0 0 0 0 0 295 5 0 0 0 0 0 0 0 0 0 0 0 0 107 0 582 0 0 0 0 send_
F 1297 5 1292 1293 1294 1295 1296
S 1298 23 5 0 0 0 1304 582 5708 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 recv_
S 1299 1 3 2 0 194 1 1298 7269 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav
S 1300 1 3 1 0 6 1 1298 5901 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 dest
S 1301 1 3 1 0 6 1 1298 5906 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tagbase
S 1302 1 3 1 0 6 1 1298 5085 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1303 1 3 2 0 6 1 1298 5894 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 status
S 1304 14 5 0 0 0 1 1298 5708 10 400000 A 0 0 0 0 0 0 0 301 5 0 0 0 0 0 0 0 0 0 0 0 0 267 0 582 0 0 0 0 recv_
F 1304 5 1299 1300 1301 1302 1303
R 1308 25 1 m_globalmap globalmap
R 1309 5 2 m_globalmap comp_id globalmap
R 1310 5 3 m_globalmap gsize globalmap
R 1311 5 4 m_globalmap lsize globalmap
R 1313 5 6 m_globalmap counts globalmap
R 1314 5 7 m_globalmap counts$sd globalmap
R 1315 5 8 m_globalmap counts$p globalmap
R 1316 5 9 m_globalmap counts$o globalmap
R 1319 5 12 m_globalmap displs globalmap
R 1320 5 13 m_globalmap displs$sd globalmap
R 1321 5 14 m_globalmap displs$p globalmap
R 1322 5 15 m_globalmap displs$o globalmap
R 1324 19 17 m_globalmap gsize
R 1325 19 18 m_globalmap lsize
R 1326 19 19 m_globalmap init
R 1327 19 20 m_globalmap init_remote
R 1328 19 21 m_globalmap clean
R 1329 19 22 m_globalmap rank
R 1330 19 23 m_globalmap bounds
R 1331 19 24 m_globalmap comp_id
S 1400 23 5 0 0 0 1407 582 7195 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gm_gather_
S 1401 1 3 1 0 194 1 1400 7649 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iv
S 1402 1 3 2 0 194 1 1400 7652 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ov
S 1403 1 3 1 0 322 1 1400 7547 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 1404 1 3 1 0 6 1 1400 5080 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1405 1 3 1 0 6 1 1400 5085 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1406 1 3 2 0 6 1 1400 5090 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1407 14 5 0 0 0 1 1400 7195 10 400000 A 0 0 0 0 0 0 0 343 6 0 0 0 0 0 0 0 0 0 0 0 0 424 0 582 0 0 0 0 gm_gather_
F 1407 6 1401 1402 1403 1404 1405 1406
R 1410 25 1 m_globalsegmap globalsegmap
R 1411 19 2 m_globalsegmap init
R 1412 19 3 m_globalsegmap clean
R 1413 5 4 m_globalsegmap comp_id globalsegmap
R 1414 5 5 m_globalsegmap gsize globalsegmap
R 1415 19 6 m_globalsegmap globalstorage
R 1416 19 7 m_globalsegmap processstorage
R 1417 19 8 m_globalsegmap orderedpoints
R 1418 19 9 m_globalsegmap lsize
R 1419 5 10 m_globalsegmap ngseg globalsegmap
R 1420 19 11 m_globalsegmap nlseg
R 1421 19 12 m_globalsegmap max_nlseg
R 1422 19 13 m_globalsegmap active_pes
R 1423 19 14 m_globalsegmap pelocs
R 1424 19 15 m_globalsegmap haloed
R 1425 19 16 m_globalsegmap rank
R 1426 19 17 m_globalsegmap sort
R 1427 19 18 m_globalsegmap permute
R 1428 19 19 m_globalsegmap sortpermute
R 1429 19 20 m_globalsegmap increasing
R 1430 19 21 m_globalsegmap copy
R 1432 5 23 m_globalsegmap start globalsegmap
R 1433 5 24 m_globalsegmap start$sd globalsegmap
R 1434 5 25 m_globalsegmap start$p globalsegmap
R 1435 5 26 m_globalsegmap start$o globalsegmap
R 1438 5 29 m_globalsegmap length globalsegmap
R 1439 5 30 m_globalsegmap length$sd globalsegmap
R 1440 5 31 m_globalsegmap length$p globalsegmap
R 1441 5 32 m_globalsegmap length$o globalsegmap
R 1444 5 35 m_globalsegmap pe_loc globalsegmap
R 1445 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1446 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1447 5 38 m_globalsegmap pe_loc$o globalsegmap
R 1456 19 47 m_globalsegmap comp_id
R 1458 19 49 m_globalsegmap gsize
R 1464 19 55 m_globalsegmap ngseg
S 1699 23 5 0 0 0 1708 582 7206 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsm_gather_
S 1700 1 3 1 0 194 1 1699 7649 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iv
S 1701 1 3 2 0 194 1 1699 7652 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ov
S 1702 1 3 1 0 440 1 1699 8179 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 1703 1 3 1 0 6 1 1699 5080 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1704 1 3 1 0 6 1 1699 5085 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1705 1 3 2 0 6 1 1699 5090 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1706 1 3 1 0 9 1 1699 8487 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rdefault
S 1707 1 3 1 0 6 1 1699 8496 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 idefault
S 1708 14 5 0 0 0 1 1699 7206 10 400000 A 0 0 0 0 0 0 0 459 8 0 0 0 0 0 0 0 0 0 0 0 0 599 0 582 0 0 0 0 gsm_gather_
F 1708 8 1700 1701 1702 1703 1704 1705 1706 1707
S 1709 23 5 0 0 0 1716 582 7218 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gm_scatter_
S 1710 1 3 1 0 194 1 1709 7649 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iv
S 1711 1 3 2 0 194 1 1709 7652 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ov
S 1712 1 3 1 0 291 1 1709 7547 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 1713 1 3 1 0 6 1 1709 5080 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1714 1 3 1 0 6 1 1709 5085 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1715 1 3 2 0 6 1 1709 5090 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1716 14 5 0 0 0 1 1709 7218 10 400000 A 0 0 0 0 0 0 0 468 6 0 0 0 0 0 0 0 0 0 0 0 0 892 0 582 0 0 0 0 gm_scatter_
F 1716 6 1710 1711 1712 1713 1714 1715
S 1717 23 5 0 0 0 1724 582 7230 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsm_scatter_
S 1718 1 3 1 0 194 1 1717 7649 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iv
S 1719 1 3 2 0 194 1 1717 7652 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ov
S 1720 1 3 1 0 328 1 1717 8179 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 1721 1 3 1 0 6 1 1717 5080 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1722 1 3 1 0 6 1 1717 5085 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1723 1 3 2 0 6 1 1717 5090 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1724 14 5 0 0 0 1 1717 7230 10 400000 A 0 0 0 0 0 0 0 475 6 0 0 0 0 0 0 0 0 0 0 0 0 1145 0 582 0 0 0 0 gsm_scatter_
F 1724 6 1718 1719 1720 1721 1722 1723
S 1725 23 5 0 0 0 1730 582 4908 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bcast_
S 1726 1 3 3 0 194 1 1725 6859 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 av
S 1727 1 3 1 0 6 1 1725 5080 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1728 1 3 1 0 6 1 1725 5085 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1729 1 3 2 0 6 1 1725 5090 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1730 14 5 0 0 0 1 1725 4908 10 400000 A 0 0 0 0 0 0 0 482 4 0 0 0 0 0 0 0 0 0 0 0 0 1504 0 582 0 0 0 0 bcast_
F 1730 4 1726 1727 1728 1729
A 115 2 0 0 0 6 757 0 0 0 115 0 0 0 0 0 0 0 0 0
A 306 2 0 0 167 289 1289 0 0 0 306 0 0 0 0 0 0 0 0 0
Z
Z
