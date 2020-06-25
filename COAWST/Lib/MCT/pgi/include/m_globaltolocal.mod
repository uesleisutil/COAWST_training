V26 m_globaltolocal
19 m_GlobalToLocal.F90 S582 0
03/26/2019  11:41:38
use m_attrvect private
use m_list private
use m_string private
use m_sparsematrix private
use m_navigator private
use m_globalsegmap private
use m_globalmap private
use m_attrvect private
use m_list private
use m_string private
use m_sparsematrix private
use m_navigator private
use m_globalsegmap private
use m_globalmap private
enduse
D 56 18 12
D 58 24 606 280 603 7
D 170 24 606 280 603 7
D 176 21 6 1 156 162 0 1 0 0 1
 157 160 161 157 160 158
D 179 21 6 1 0 18 0 0 0 0 0
 0 18 0 3 18 0
D 182 21 6 1 164 170 0 1 0 0 1
 165 168 169 165 168 166
D 185 21 6 1 0 18 0 0 0 0 0
 0 18 0 3 18 0
D 188 21 6 1 171 174 1 1 0 0 1
 3 172 3 3 172 173
D 191 21 6 1 175 178 1 1 0 0 1
 3 176 3 3 176 177
D 225 24 931 192 930 7
D 266 24 1031 184 1030 7
D 664 24 1817 1056 1816 7
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 25 0 0 0 0 0 0 m_globaltolocal
S 583 19 0 0 0 8 1 582 4674 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 2 0 0 0 0 0 582 0 0 0 0 globaltolocalindex
O 583 2 590 589
S 584 19 0 0 0 8 1 582 4693 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 3 0 0 0 0 0 582 0 0 0 0 globaltolocalindices
O 584 3 588 587 586
S 585 19 0 0 0 8 1 582 4714 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 12 1 0 0 0 0 0 582 0 0 0 0 globaltolocalmatrix
O 585 1 591
S 586 27 0 0 0 8 892 582 4734 10010 400000 A 0 0 0 0 0 0 65 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptoindices_
Q 586 584 0
S 587 27 0 0 0 8 1115 582 4757 10010 400000 A 0 0 0 0 0 0 107 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptonavigator_
Q 587 584 0
S 588 27 0 0 0 8 912 582 4782 10010 400000 A 0 0 0 0 0 0 67 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptoindexarr_
Q 588 584 0
S 589 27 0 0 0 8 906 582 4806 10010 400000 A 0 0 0 0 0 0 66 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptoindex_
Q 589 583 0
S 590 27 0 0 0 8 1022 582 4827 10010 400000 A 0 0 0 0 0 0 86 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalmaptoindex_
Q 590 583 0
S 591 27 0 0 0 8 2140 582 4845 10010 400000 A 0 0 0 0 0 0 350 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 globalsegmaptolocalmatrix_
Q 591 585 0
S 592 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 593 3 0 0 0 56 0 1 0 0 0 A 0 0 0 0 0 0 0 0 4872 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 20 4d 43 54 3a 3a 6d 5f 47 6c 6f 62 61 6c 54 6f 4c 6f 63 61 6c
S 594 16 0 0 0 56 1 582 4893 14 440000 A 0 0 0 0 0 0 0 0 593 13 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 596 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 597 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 598 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 599 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 600 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 601 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 603 25 1 m_globalsegmap globalsegmap
R 604 19 2 m_globalsegmap init
R 605 19 3 m_globalsegmap clean
R 606 5 4 m_globalsegmap comp_id globalsegmap
R 607 5 5 m_globalsegmap gsize globalsegmap
R 608 19 6 m_globalsegmap globalstorage
R 609 19 7 m_globalsegmap processstorage
R 610 19 8 m_globalsegmap orderedpoints
R 611 19 9 m_globalsegmap lsize
R 612 5 10 m_globalsegmap ngseg globalsegmap
R 613 19 11 m_globalsegmap nlseg
R 614 19 12 m_globalsegmap max_nlseg
R 615 19 13 m_globalsegmap active_pes
R 616 19 14 m_globalsegmap pelocs
R 617 19 15 m_globalsegmap haloed
R 618 19 16 m_globalsegmap rank
R 619 19 17 m_globalsegmap sort
R 620 19 18 m_globalsegmap permute
R 621 19 19 m_globalsegmap sortpermute
R 622 19 20 m_globalsegmap increasing
R 623 19 21 m_globalsegmap copy
R 625 5 23 m_globalsegmap start globalsegmap
R 626 5 24 m_globalsegmap start$sd globalsegmap
R 627 5 25 m_globalsegmap start$p globalsegmap
R 628 5 26 m_globalsegmap start$o globalsegmap
R 631 5 29 m_globalsegmap length globalsegmap
R 632 5 30 m_globalsegmap length$sd globalsegmap
R 633 5 31 m_globalsegmap length$p globalsegmap
R 634 5 32 m_globalsegmap length$o globalsegmap
R 637 5 35 m_globalsegmap pe_loc globalsegmap
R 638 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 639 5 37 m_globalsegmap pe_loc$p globalsegmap
R 640 5 38 m_globalsegmap pe_loc$o globalsegmap
R 649 19 47 m_globalsegmap comp_id
R 651 19 49 m_globalsegmap gsize
R 657 19 55 m_globalsegmap ngseg
S 892 23 5 0 0 0 897 582 4734 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptoindices_
S 893 1 3 1 0 170 1 892 5564 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 894 1 3 1 0 6 1 892 5780 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 895 7 3 0 0 176 1 892 5116 10800014 3014 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 898 0 0 0 0 0 0 0 0 start
S 896 7 3 0 0 182 1 892 3869 10800014 3014 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 902 0 0 0 0 0 0 0 0 length
S 897 14 5 0 0 0 1 892 4734 10 400000 A 0 0 0 0 0 0 0 111 4 0 0 0 0 0 0 0 0 0 0 0 0 98 0 582 0 0 0 0 globalsegmaptoindices_
F 897 4 893 894 895 896
S 898 8 1 0 0 179 1 892 5951 40822014 1020 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 start$sd1
S 902 8 1 0 0 185 1 892 5993 40822014 1020 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 length$sd5
S 906 23 5 0 0 6 910 582 4806 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptoindex_
S 907 1 3 1 0 58 1 906 5564 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 908 1 3 1 0 6 1 906 5785 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 i_g
S 909 1 3 1 0 6 1 906 5780 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 910 14 5 0 0 6 1 906 4806 14 400000 A 0 0 0 0 0 0 0 116 3 0 0 911 0 0 0 0 0 0 0 0 0 201 0 582 0 0 0 0 globalsegmaptoindex_
F 910 3 907 908 909
S 911 1 3 0 0 6 1 906 4806 14 1003000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptoindex_
S 912 23 5 0 0 0 918 582 4782 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptoindexarr_
S 913 1 3 1 0 58 1 912 5564 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 914 7 3 1 0 188 1 912 6039 20000014 10003000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 i_global
S 915 7 3 2 0 191 1 912 6048 20000014 10003000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 i_local
S 916 1 3 1 0 6 1 912 2037 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nindex
S 917 1 3 1 0 6 1 912 5780 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 918 14 5 0 0 0 1 912 4782 20000010 400000 A 0 0 0 0 0 0 0 120 5 0 0 0 0 0 0 0 0 0 0 0 0 320 0 582 0 0 0 0 globalsegmaptoindexarr_
F 918 5 913 914 915 916 917
S 919 6 1 0 0 6 1 912 5589 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_0_1
S 920 6 1 0 0 6 1 912 5597 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_2_1
S 921 6 1 0 0 6 1 912 5605 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_3
S 922 6 1 0 0 6 1 912 6056 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_652
S 923 6 1 0 0 6 1 912 5619 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_4
S 924 6 1 0 0 6 1 912 5625 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_6
S 925 6 1 0 0 6 1 912 5631 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_7
S 926 6 1 0 0 6 1 912 6064 40800016 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_659
R 930 25 1 m_globalmap globalmap
R 931 5 2 m_globalmap comp_id globalmap
R 932 5 3 m_globalmap gsize globalmap
R 933 5 4 m_globalmap lsize globalmap
R 935 5 6 m_globalmap counts globalmap
R 936 5 7 m_globalmap counts$sd globalmap
R 937 5 8 m_globalmap counts$p globalmap
R 938 5 9 m_globalmap counts$o globalmap
R 941 5 12 m_globalmap displs globalmap
R 942 5 13 m_globalmap displs$sd globalmap
R 943 5 14 m_globalmap displs$p globalmap
R 944 5 15 m_globalmap displs$o globalmap
R 946 19 17 m_globalmap gsize
R 947 19 18 m_globalmap lsize
R 948 19 19 m_globalmap init
R 949 19 20 m_globalmap init_remote
R 950 19 21 m_globalmap clean
R 951 19 22 m_globalmap rank
R 952 19 23 m_globalmap bounds
R 953 19 24 m_globalmap comp_id
S 1022 23 5 0 0 6 1026 582 4827 14 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalmaptoindex_
S 1023 1 3 1 0 225 1 1022 6295 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 1024 1 3 1 0 6 1 1022 5785 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 i_g
S 1025 1 3 1 0 6 1 1022 5780 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1026 14 5 0 0 6 1 1022 4827 14 400000 A 0 0 0 0 0 0 0 162 3 0 0 1027 0 0 0 0 0 0 0 0 0 476 0 582 0 0 0 0 globalmaptoindex_
F 1026 3 1023 1024 1025
S 1027 1 3 0 0 6 1 1022 4827 14 1003000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalmaptoindex_
R 1030 25 1 m_navigator navigator
R 1031 5 2 m_navigator numsegments navigator
R 1032 5 3 m_navigator vectorlength navigator
R 1034 5 5 m_navigator displs navigator
R 1035 5 6 m_navigator displs$sd navigator
R 1036 5 7 m_navigator displs$p navigator
R 1037 5 8 m_navigator displs$o navigator
R 1040 5 11 m_navigator counts navigator
R 1041 5 12 m_navigator counts$sd navigator
R 1042 5 13 m_navigator counts$p navigator
R 1043 5 14 m_navigator counts$o navigator
R 1045 19 16 m_navigator navigator_init
R 1046 19 17 m_navigator init
R 1047 19 18 m_navigator clean
R 1048 19 19 m_navigator numsegments
R 1049 19 20 m_navigator vectorlength
R 1050 19 21 m_navigator msize
R 1051 19 22 m_navigator resize
R 1052 19 23 m_navigator get
R 1053 19 24 m_navigator ptr_displs
R 1054 19 25 m_navigator ptr_counts
S 1115 23 5 0 0 0 1119 582 4757 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptonavigator_
S 1116 1 3 1 0 58 1 1115 5564 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 1117 1 3 1 0 6 1 1115 5780 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1118 1 3 2 0 266 1 1115 6671 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 onav
S 1119 14 5 0 0 0 1 1115 4757 10 400000 A 0 0 0 0 0 0 0 198 3 0 0 0 0 0 0 0 0 0 0 0 0 551 0 582 0 0 0 0 globalsegmaptonavigator_
F 1119 3 1116 1117 1118
R 1144 19 8 m_string tochar
R 1145 19 9 m_string string_init
R 1146 19 10 m_string init
R 1147 19 11 m_string string_clean
R 1148 19 12 m_string clean
R 1149 19 13 m_string string_len
R 1150 19 14 m_string string_bcast
R 1151 19 15 m_string bcast
R 1152 19 16 m_string string_mci
R 1153 19 17 m_string string_mco
R 1154 19 18 m_string ptr_chars
R 1155 19 19 m_string char
R 1163 19 27 m_string len
R 1317 19 15 m_list init
R 1318 19 16 m_list clean
R 1319 19 17 m_list nullify
R 1320 19 18 m_list get_indices
R 1321 19 19 m_list test_indices
R 1322 19 20 m_list nitem
R 1323 19 21 m_list get
R 1324 19 22 m_list identical
R 1326 19 24 m_list copy
R 1327 19 25 m_list exporttochar
R 1328 19 26 m_list exporttostring
R 1329 19 27 m_list charbuffersize
R 1330 19 28 m_list append
R 1331 19 29 m_list concatenate
R 1332 19 30 m_list bcast
R 1333 19 31 m_list send
R 1334 19 32 m_list recv
R 1335 19 33 m_list getsharedlistindices
R 1341 19 39 m_list index
R 1352 19 50 m_list allocated
R 1517 19 22 m_attrvect init
R 1518 19 23 m_attrvect clean
R 1519 19 24 m_attrvect zero
R 1520 19 25 m_attrvect lsize
R 1521 19 26 m_attrvect niattr
R 1522 19 27 m_attrvect nrattr
R 1523 19 28 m_attrvect indexia
R 1524 19 29 m_attrvect indexra
R 1525 19 30 m_attrvect getilist
R 1526 19 31 m_attrvect getrlist
R 1527 19 32 m_attrvect exportilist
R 1528 19 33 m_attrvect exportrlist
R 1529 19 34 m_attrvect exportilisttochar
R 1530 19 35 m_attrvect exportrlisttochar
R 1531 19 36 m_attrvect appendiattr
R 1532 19 37 m_attrvect appendrattr
R 1533 19 38 m_attrvect exportiattr
R 1534 19 39 m_attrvect exportrattr
R 1535 19 40 m_attrvect importiattr
R 1536 19 41 m_attrvect importrattr
R 1537 19 42 m_attrvect copy
R 1538 19 43 m_attrvect rcopy
R 1539 19 44 m_attrvect icopy
R 1540 19 45 m_attrvect sort
R 1541 19 46 m_attrvect permute
R 1542 19 47 m_attrvect unpermute
R 1543 19 48 m_attrvect sortpermute
R 1544 19 49 m_attrvect sharedattrindexlist
R 1816 25 3 m_sparsematrix sparsematrix
R 1817 5 4 m_sparsematrix nrows sparsematrix
R 1818 5 5 m_sparsematrix ncols sparsematrix
R 1819 5 6 m_sparsematrix data sparsematrix
R 1820 5 7 m_sparsematrix vecinit sparsematrix
R 1822 5 9 m_sparsematrix row_s sparsematrix
R 1823 5 10 m_sparsematrix row_s$sd sparsematrix
R 1824 5 11 m_sparsematrix row_s$p sparsematrix
R 1825 5 12 m_sparsematrix row_s$o sparsematrix
R 1827 5 14 m_sparsematrix row_e sparsematrix
R 1829 5 16 m_sparsematrix row_e$sd sparsematrix
R 1830 5 17 m_sparsematrix row_e$p sparsematrix
R 1831 5 18 m_sparsematrix row_e$o sparsematrix
R 1835 5 22 m_sparsematrix tcol sparsematrix
R 1836 5 23 m_sparsematrix tcol$sd sparsematrix
R 1837 5 24 m_sparsematrix tcol$p sparsematrix
R 1838 5 25 m_sparsematrix tcol$o sparsematrix
R 1842 5 29 m_sparsematrix twgt sparsematrix
R 1843 5 30 m_sparsematrix twgt$sd sparsematrix
R 1844 5 31 m_sparsematrix twgt$p sparsematrix
R 1845 5 32 m_sparsematrix twgt$o sparsematrix
R 1847 5 34 m_sparsematrix row_max sparsematrix
R 1848 5 35 m_sparsematrix row_min sparsematrix
R 1849 5 36 m_sparsematrix tbl_end sparsematrix
R 1850 19 37 m_sparsematrix init
R 1851 19 38 m_sparsematrix vecinit
R 1852 19 39 m_sparsematrix clean
R 1853 19 40 m_sparsematrix lsize
R 1854 19 41 m_sparsematrix indexia
R 1855 19 42 m_sparsematrix indexra
R 1856 19 43 m_sparsematrix nrows
R 1857 19 44 m_sparsematrix ncols
R 1858 19 45 m_sparsematrix exportglobalrowindices
R 1859 19 46 m_sparsematrix exportglobalcolumnindices
R 1860 19 47 m_sparsematrix exportlocalrowindices
R 1861 19 48 m_sparsematrix exportlocalcolumnindices
R 1862 19 49 m_sparsematrix exportmatrixelements
R 1863 19 50 m_sparsematrix importglobalrowindices
R 1864 19 51 m_sparsematrix importglobalcolumnindices
R 1865 19 52 m_sparsematrix importlocalrowindices
R 1866 19 53 m_sparsematrix importlocalcolumnindices
R 1867 19 54 m_sparsematrix importmatrixelements
R 1868 19 55 m_sparsematrix copy
R 1869 19 56 m_sparsematrix globalnumelements
R 1870 19 57 m_sparsematrix computesparsity
R 1871 19 58 m_sparsematrix local_row_range
R 1872 19 59 m_sparsematrix global_row_range
R 1873 19 60 m_sparsematrix local_col_range
R 1874 19 61 m_sparsematrix global_col_range
R 1875 19 62 m_sparsematrix checkbounds
R 1876 19 63 m_sparsematrix row_sum
R 1877 19 64 m_sparsematrix row_sum_check
R 1878 19 65 m_sparsematrix sort
R 1879 19 66 m_sparsematrix permute
R 1880 19 67 m_sparsematrix sortpermute
S 2140 23 5 0 0 0 2145 582 4845 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 globalsegmaptolocalmatrix_
S 2141 1 3 3 0 664 1 2140 10364 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smat
S 2142 1 3 1 0 58 1 2140 5564 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 2143 1 3 1 0 28 1 2140 10803 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rcflag
S 2144 1 3 1 0 6 1 2140 5780 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 2145 14 5 0 0 0 1 2140 4845 10 400000 A 0 0 0 0 0 0 0 640 4 0 0 0 0 0 0 0 0 0 0 0 0 628 0 582 0 0 0 0 globalsegmaptolocalmatrix_
F 2145 4 2141 2142 2143 2144
A 12 2 0 0 0 6 592 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 2 0 0 0 56 593 0 0 0 13 0 0 0 0 0 0 0 0 0
A 15 2 0 0 0 6 598 0 0 0 15 0 0 0 0 0 0 0 0 0
A 18 2 0 0 0 6 596 0 0 0 18 0 0 0 0 0 0 0 0 0
A 20 2 0 0 0 6 601 0 0 0 20 0 0 0 0 0 0 0 0 0
A 22 2 0 0 0 6 597 0 0 0 22 0 0 0 0 0 0 0 0 0
A 27 2 0 0 0 6 599 0 0 0 27 0 0 0 0 0 0 0 0 0
A 29 2 0 0 0 6 600 0 0 0 29 0 0 0 0 0 0 0 0 0
A 155 1 0 1 0 179 898 0 0 0 0 0 0 0 0 0 0 0 0 0
A 156 10 0 0 0 6 155 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 20
A 157 10 0 0 156 6 155 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 22
A 158 10 0 0 157 6 155 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 15
A 159 4 0 0 0 6 158 0 3 0 0 0 0 2 0 0 0 0 0 0
A 160 4 0 0 7 6 157 0 159 0 0 0 0 1 0 0 0 0 0 0
A 161 10 0 0 158 6 155 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 27
A 162 10 0 0 161 6 155 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 29
A 163 1 0 1 0 185 902 0 0 0 0 0 0 0 0 0 0 0 0 0
A 164 10 0 0 0 6 163 1 0 0 0 0 0 0 0 0 0 0 0 0
X 1 20
A 165 10 0 0 164 6 163 4 0 0 0 0 0 0 0 0 0 0 0 0
X 1 22
A 166 10 0 0 165 6 163 7 0 0 0 0 0 0 0 0 0 0 0 0
X 1 15
A 167 4 0 0 0 6 166 0 3 0 0 0 0 2 0 0 0 0 0 0
A 168 4 0 0 0 6 165 0 167 0 0 0 0 1 0 0 0 0 0 0
A 169 10 0 0 166 6 163 10 0 0 0 0 0 0 0 0 0 0 0 0
X 1 27
A 170 10 0 0 169 6 163 13 0 0 0 0 0 0 0 0 0 0 0 0
X 1 29
A 171 1 0 0 0 6 921 0 0 0 0 0 0 0 0 0 0 0 0 0
A 172 1 0 0 0 6 919 0 0 0 0 0 0 0 0 0 0 0 0 0
A 173 1 0 0 0 6 922 0 0 0 0 0 0 0 0 0 0 0 0 0
A 174 1 0 0 0 6 920 0 0 0 0 0 0 0 0 0 0 0 0 0
A 175 1 0 0 0 6 925 0 0 0 0 0 0 0 0 0 0 0 0 0
A 176 1 0 0 0 6 923 0 0 0 0 0 0 0 0 0 0 0 0 0
A 177 1 0 0 0 6 926 0 0 0 0 0 0 0 0 0 0 0 0 0
A 178 1 0 0 0 6 924 0 0 0 0 0 0 0 0 0 0 0 0 0
Z
Z
