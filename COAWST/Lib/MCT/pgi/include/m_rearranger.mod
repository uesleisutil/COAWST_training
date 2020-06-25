V26 m_rearranger
16 m_Rearranger.F90 S582 0
03/26/2019  11:41:40
use m_list private
use m_string private
use m_attrvect private
use m_globalsegmap private
use m_router private
use m_list private
use m_string private
use m_attrvect private
use m_globalsegmap private
use m_router private
enduse
D 56 24 596 280 593 7
D 196 24 917 1184 916 7
D 299 24 1034 2488 1033 7
D 305 21 6 2 319 318 0 1 0 0 1
 308 311 316 308 311 309
 312 315 317 312 315 313
D 308 21 6 1 0 217 0 0 0 0 0
 0 217 0 3 217 0
D 311 18 24
D 313 21 6 1 0 3 0 0 0 0 0
 0 3 0 3 3 0
D 543 24 1433 624 1432 7
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 29 0 0 0 0 0 0 m_rearranger
S 584 23 0 0 0 8 916 582 4680 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 router
S 587 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 588 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 589 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 590 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 591 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 593 25 1 m_globalsegmap globalsegmap
R 594 19 2 m_globalsegmap init
R 595 19 3 m_globalsegmap clean
R 596 5 4 m_globalsegmap comp_id globalsegmap
R 597 5 5 m_globalsegmap gsize globalsegmap
R 598 19 6 m_globalsegmap globalstorage
R 599 19 7 m_globalsegmap processstorage
R 600 19 8 m_globalsegmap orderedpoints
R 601 19 9 m_globalsegmap lsize
R 602 5 10 m_globalsegmap ngseg globalsegmap
R 603 19 11 m_globalsegmap nlseg
R 604 19 12 m_globalsegmap max_nlseg
R 605 19 13 m_globalsegmap active_pes
R 606 19 14 m_globalsegmap pelocs
R 607 19 15 m_globalsegmap haloed
R 608 19 16 m_globalsegmap rank
R 609 19 17 m_globalsegmap sort
R 610 19 18 m_globalsegmap permute
R 611 19 19 m_globalsegmap sortpermute
R 612 19 20 m_globalsegmap increasing
R 613 19 21 m_globalsegmap copy
R 615 5 23 m_globalsegmap start globalsegmap
R 616 5 24 m_globalsegmap start$sd globalsegmap
R 617 5 25 m_globalsegmap start$p globalsegmap
R 618 5 26 m_globalsegmap start$o globalsegmap
R 621 5 29 m_globalsegmap length globalsegmap
R 622 5 30 m_globalsegmap length$sd globalsegmap
R 623 5 31 m_globalsegmap length$p globalsegmap
R 624 5 32 m_globalsegmap length$o globalsegmap
R 627 5 35 m_globalsegmap pe_loc globalsegmap
R 628 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 629 5 37 m_globalsegmap pe_loc$p globalsegmap
R 630 5 38 m_globalsegmap pe_loc$o globalsegmap
R 639 19 47 m_globalsegmap comp_id
R 641 19 49 m_globalsegmap gsize
R 647 19 55 m_globalsegmap ngseg
S 896 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 897 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 19 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 898 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 899 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 916 25 16 m_router router
R 917 5 17 m_router comp1id router
R 918 5 18 m_router comp2id router
R 919 5 19 m_router nprocs router
R 920 5 20 m_router maxsize router
R 921 5 21 m_router lavsize router
R 922 5 22 m_router numiatt router
R 923 5 23 m_router numratt router
R 925 5 25 m_router pe_list router
R 926 5 26 m_router pe_list$sd router
R 927 5 27 m_router pe_list$p router
R 928 5 28 m_router pe_list$o router
R 931 5 31 m_router num_segs router
R 932 5 32 m_router num_segs$sd router
R 933 5 33 m_router num_segs$p router
R 934 5 34 m_router num_segs$o router
R 937 5 37 m_router locsize router
R 938 5 38 m_router locsize$sd router
R 939 5 39 m_router locsize$p router
R 940 5 40 m_router locsize$o router
R 943 5 43 m_router permarr router
R 944 5 44 m_router permarr$sd router
R 945 5 45 m_router permarr$p router
R 946 5 46 m_router permarr$o router
R 950 5 50 m_router seg_starts router
R 951 5 51 m_router seg_starts$sd router
R 952 5 52 m_router seg_starts$p router
R 953 5 53 m_router seg_starts$o router
R 957 5 57 m_router seg_lengths router
R 958 5 58 m_router seg_lengths$sd router
R 959 5 59 m_router seg_lengths$p router
R 960 5 60 m_router seg_lengths$o router
R 963 5 63 m_router rp1 router
R 964 5 64 m_router rp1$sd router
R 965 5 65 m_router rp1$p router
R 966 5 66 m_router rp1$o router
R 969 5 69 m_router ip1 router
R 970 5 70 m_router ip1$sd router
R 971 5 71 m_router ip1$p router
R 972 5 72 m_router ip1$o router
R 975 5 75 m_router ireqs router
R 976 5 76 m_router ireqs$sd router
R 977 5 77 m_router ireqs$p router
R 978 5 78 m_router ireqs$o router
R 980 5 80 m_router rreqs router
R 982 5 82 m_router rreqs$sd router
R 983 5 83 m_router rreqs$p router
R 984 5 84 m_router rreqs$o router
R 988 5 88 m_router istatus router
R 989 5 89 m_router istatus$sd router
R 990 5 90 m_router istatus$p router
R 991 5 91 m_router istatus$o router
R 993 5 93 m_router rstatus router
R 996 5 96 m_router rstatus$sd router
R 997 5 97 m_router rstatus$p router
R 998 5 98 m_router rstatus$o router
R 1000 19 100 m_router init
R 1001 19 101 m_router clean
R 1002 19 102 m_router print
S 1033 25 0 0 0 299 1 582 6753 10000004 800014 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1057 0 0 0 582 0 0 0 0 rearranger
S 1034 5 0 0 0 196 1035 582 6764 800014 0 A 0 0 0 0 0 0 0 0 299 0 0 0 0 0 0 0 0 0 0 0 1 1034 0 582 0 0 0 0 sendrouter
S 1035 5 0 0 0 196 1038 582 6775 800014 0 A 0 0 0 0 0 1184 0 0 299 0 0 0 0 0 0 0 0 0 0 0 1034 1035 0 582 0 0 0 0 recvrouter
S 1036 6 4 0 0 6 1037 582 5383 40800016 0 A 0 0 0 0 0 0 0 0 0 0 0 0 1058 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 z_b_0_1
S 1037 6 4 0 0 6 1044 582 6786 40800016 0 A 0 0 0 0 0 4 0 0 0 0 0 0 1058 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 z_b_1_1
S 1038 5 6 0 0 305 1040 582 6794 10a00014 14 A 0 0 0 0 0 2368 1040 0 299 0 1042 0 0 0 0 0 0 0 0 1039 1035 1038 1041 582 0 0 0 0 localpack
S 1039 5 0 0 0 308 1043 582 6804 40822004 1020 A 0 0 0 0 0 2384 0 0 299 0 0 0 0 0 0 0 0 0 0 0 1041 1039 0 582 0 0 0 0 localpack$sd
S 1040 5 0 0 0 7 1041 582 6817 40802001 1020 A 0 0 0 0 0 2368 0 0 299 0 0 0 0 0 0 0 0 0 0 0 1038 1040 0 582 0 0 0 0 localpack$p
S 1041 5 0 0 0 7 1039 582 6829 40802000 1020 A 0 0 0 0 0 2376 0 0 299 0 0 0 0 0 0 0 0 0 0 0 1040 1041 0 582 0 0 0 0 localpack$o
S 1042 22 1 0 0 6 1 582 6841 40000000 1000 A 0 0 0 0 0 0 0 1038 0 0 0 0 1039 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 localpack$arrdsc
S 1043 5 0 0 0 6 1 582 6858 800014 0 A 0 0 0 0 0 2480 0 0 299 0 0 0 0 0 0 0 0 0 0 0 1038 1043 0 582 0 0 0 0 localsize
S 1044 6 4 0 0 6 1 582 6868 14 0 A 0 0 0 0 0 8 0 0 0 0 0 0 1058 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 max_nprocs
S 1045 19 0 0 0 6 1 582 4730 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 63 1 0 0 0 0 0 582 0 0 0 0 init
O 1045 1 1049
S 1046 19 0 0 0 8 1 582 6879 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 65 1 0 0 0 0 0 582 0 0 0 0 rearrange
O 1046 1 1050
S 1047 19 0 0 0 8 1 582 4735 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 67 1 0 0 0 0 0 582 0 0 0 0 clean
O 1047 1 1051
S 1048 19 0 0 0 8 1 582 6632 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 69 1 0 0 0 0 0 582 0 0 0 0 print
O 1048 1 1052
S 1049 27 0 0 0 6 1059 582 6889 10010 400000 A 0 0 0 0 0 0 70 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 init_
Q 1049 1045 0
S 1050 27 0 0 0 8 1739 582 6895 10010 400000 A 0 0 0 0 0 0 242 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 rearrange_
Q 1050 1046 0
S 1051 27 0 0 0 8 1065 582 5099 10010 400000 A 0 0 0 0 0 0 71 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 clean_
Q 1051 1047 0
S 1052 27 0 0 0 8 1748 582 6638 10010 400000 A 0 0 0 0 0 0 243 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 print_
Q 1052 1048 0
S 1053 16 0 0 0 6 1 582 6906 14 400000 A 0 0 0 0 0 0 0 0 500 320 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 defaulttag
S 1054 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 500 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 1055 3 0 0 0 311 0 1 0 0 0 A 0 0 0 0 0 0 0 0 6917 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 17 4d 43 54 3a 3a 6d 5f 52 65 61 72 72 61 6e 67 65 72
S 1056 16 0 0 0 311 1 582 5302 14 440000 A 0 0 0 0 0 0 0 0 1055 322 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 1057 8 5 0 0 313 1 582 6935 40022004 1220 A 0 0 0 0 0 0 0 299 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 m_rearranger$rearranger$td
S 1058 11 0 0 0 8 1011 582 6962 40800010 801000 A 0 0 0 0 0 12 0 0 1036 1044 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _m_rearranger$4
S 1059 23 5 0 0 0 1064 582 6889 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 init_
S 1060 1 3 1 0 56 1 1059 6978 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sourcegsmap
S 1061 1 3 1 0 56 1 1059 6990 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 targetgsmap
S 1062 1 3 1 0 6 1 1059 6720 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mycomm
S 1063 1 3 2 0 299 1 1059 7002 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outrearranger
S 1064 14 5 0 0 0 1 1059 6889 10 400000 A 0 0 0 0 0 0 0 128 4 0 0 0 0 0 0 0 0 0 0 0 0 108 0 582 0 0 0 0 init_
F 1064 4 1060 1061 1062 1063
S 1065 23 5 0 0 0 1068 582 5099 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 clean_
S 1066 1 3 3 0 299 1 1065 7016 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rearr
S 1067 1 3 2 0 6 1 1065 7022 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 status
S 1068 14 5 0 0 0 1 1065 5099 10 400000 A 0 0 0 0 0 0 0 133 2 0 0 0 0 0 0 0 0 0 0 0 0 425 0 582 0 0 0 0 clean_
F 1068 2 1066 1067
R 1078 19 8 m_string tochar
R 1079 19 9 m_string string_init
R 1080 19 10 m_string init
R 1081 19 11 m_string string_clean
R 1082 19 12 m_string clean
R 1083 19 13 m_string string_len
R 1084 19 14 m_string string_bcast
R 1085 19 15 m_string bcast
R 1086 19 16 m_string string_mci
R 1087 19 17 m_string string_mco
R 1088 19 18 m_string ptr_chars
R 1089 19 19 m_string char
R 1097 19 27 m_string len
R 1249 19 15 m_list init
R 1250 19 16 m_list clean
R 1251 19 17 m_list nullify
R 1252 19 18 m_list get_indices
R 1253 19 19 m_list test_indices
R 1254 19 20 m_list nitem
R 1255 19 21 m_list get
R 1256 19 22 m_list identical
R 1258 19 24 m_list copy
R 1259 19 25 m_list exporttochar
R 1260 19 26 m_list exporttostring
R 1261 19 27 m_list charbuffersize
R 1262 19 28 m_list append
R 1263 19 29 m_list concatenate
R 1264 19 30 m_list bcast
R 1265 19 31 m_list send
R 1266 19 32 m_list recv
R 1267 19 33 m_list getsharedlistindices
R 1273 19 39 m_list index
R 1284 19 50 m_list allocated
R 1432 25 5 m_attrvect attrvect
R 1433 5 6 m_attrvect ilist attrvect
R 1434 5 7 m_attrvect rlist attrvect
R 1437 5 10 m_attrvect iattr attrvect
R 1438 5 11 m_attrvect iattr$sd attrvect
R 1439 5 12 m_attrvect iattr$p attrvect
R 1440 5 13 m_attrvect iattr$o attrvect
R 1444 5 17 m_attrvect rattr attrvect
R 1445 5 18 m_attrvect rattr$sd attrvect
R 1446 5 19 m_attrvect rattr$p attrvect
R 1447 5 20 m_attrvect rattr$o attrvect
R 1449 19 22 m_attrvect init
R 1450 19 23 m_attrvect clean
R 1451 19 24 m_attrvect zero
R 1452 19 25 m_attrvect lsize
R 1453 19 26 m_attrvect niattr
R 1454 19 27 m_attrvect nrattr
R 1455 19 28 m_attrvect indexia
R 1456 19 29 m_attrvect indexra
R 1457 19 30 m_attrvect getilist
R 1458 19 31 m_attrvect getrlist
R 1459 19 32 m_attrvect exportilist
R 1460 19 33 m_attrvect exportrlist
R 1461 19 34 m_attrvect exportilisttochar
R 1462 19 35 m_attrvect exportrlisttochar
R 1463 19 36 m_attrvect appendiattr
R 1464 19 37 m_attrvect appendrattr
R 1465 19 38 m_attrvect exportiattr
R 1466 19 39 m_attrvect exportrattr
R 1467 19 40 m_attrvect importiattr
R 1468 19 41 m_attrvect importrattr
R 1469 19 42 m_attrvect copy
R 1470 19 43 m_attrvect rcopy
R 1471 19 44 m_attrvect icopy
R 1472 19 45 m_attrvect sort
R 1473 19 46 m_attrvect permute
R 1474 19 47 m_attrvect unpermute
R 1475 19 48 m_attrvect sortpermute
R 1476 19 49 m_attrvect sharedattrindexlist
S 1739 23 5 0 0 0 1747 582 6895 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rearrange_
S 1740 1 3 1 0 543 1 1739 9250 14 3008 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sourceavin
S 1741 1 3 3 0 543 1 1739 9261 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 targetav
S 1742 1 3 1 0 299 1 1739 9270 14 3008 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inrearranger
S 1743 1 3 1 0 6 1 1739 9283 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tag
S 1744 1 3 1 0 16 1 1739 2783 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sum
S 1745 1 3 1 0 16 1 1739 9110 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 vector
S 1746 1 3 1 0 16 1 1739 9287 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 alltoall
S 1747 14 5 0 0 0 1 1739 6895 10 400000 A 0 0 0 0 0 0 0 429 7 0 0 0 0 0 0 0 0 0 0 0 0 534 0 582 0 0 0 0 rearrange_
F 1747 7 1740 1741 1742 1743 1744 1745 1746
S 1748 23 5 0 0 0 1752 582 6638 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 print_
S 1749 1 3 1 0 299 1 1748 7016 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rearr
S 1750 1 3 1 0 6 1 1748 6720 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mycomm
S 1751 1 3 1 0 6 1 1748 6749 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lun
S 1752 14 5 0 0 0 1 1748 6638 10 400000 A 0 0 0 0 0 0 0 437 3 0 0 0 0 0 0 0 0 0 0 0 0 1310 0 582 0 0 0 0 print_
F 1752 3 1749 1750 1751
A 12 2 0 0 0 6 588 0 0 0 12 0 0 0 0 0 0 0 0 0
A 17 2 0 0 0 6 591 0 0 0 17 0 0 0 0 0 0 0 0 0
A 19 2 0 0 0 6 587 0 0 0 19 0 0 0 0 0 0 0 0 0
A 24 2 0 0 0 6 589 0 0 0 24 0 0 0 0 0 0 0 0 0
A 26 2 0 0 0 6 590 0 0 0 26 0 0 0 0 0 0 0 0 0
A 217 2 0 0 0 6 896 0 0 0 217 0 0 0 0 0 0 0 0 0
A 225 2 0 0 165 6 897 0 0 0 225 0 0 0 0 0 0 0 0 0
A 227 2 0 0 218 6 898 0 0 0 227 0 0 0 0 0 0 0 0 0
A 231 2 0 0 0 6 899 0 0 0 231 0 0 0 0 0 0 0 0 0
A 307 1 0 3 0 308 1039 0 0 0 0 0 0 0 0 0 0 0 0 0
A 308 10 0 0 0 6 307 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 19
A 309 10 0 0 308 6 307 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 12
A 310 4 0 0 0 6 309 0 3 0 0 0 0 2 0 0 0 0 0 0
A 311 4 0 0 0 6 308 0 310 0 0 0 0 1 0 0 0 0 0 0
A 312 10 0 0 309 6 307 16 0 0 0 0 0 0 0 0 0 0 0 0
X 1 225
A 313 10 0 0 312 6 307 19 0 0 0 0 0 0 0 0 0 0 0 0
X 1 227
A 314 4 0 0 0 6 313 0 3 0 0 0 0 2 0 0 0 0 0 0
A 315 4 0 0 0 6 312 0 314 0 0 0 0 1 0 0 0 0 0 0
A 316 10 0 0 313 6 307 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 24
A 317 10 0 0 316 6 307 22 0 0 0 0 0 0 0 0 0 0 0 0
X 1 231
A 318 10 0 0 317 6 307 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 26
A 319 10 0 0 318 6 307 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 17
A 320 2 0 0 198 6 1054 0 0 0 320 0 0 0 0 0 0 0 0 0
A 322 2 0 0 0 311 1055 0 0 0 322 0 0 0 0 0 0 0 0 0
Z
Z
