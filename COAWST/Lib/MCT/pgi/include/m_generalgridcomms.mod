V26 m_generalgridcomms
22 m_GeneralGridComms.F90 S582 0
03/26/2019  11:41:35
use m_globalmap private
use m_globalsegmap private
use m_generalgrid private
use m_attrvect private
use m_list private
use m_string private
use m_globalmap private
use m_globalsegmap private
use m_generalgrid private
use m_attrvect private
use m_list private
use m_string private
enduse
D 301 24 1284 1712 1283 7
D 450 18 134
D 452 24 1551 192 1550 7
D 483 24 1551 192 1550 7
D 489 24 1655 280 1652 7
D 601 24 1655 280 1652 7
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 m_generalgridcomms
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
S 772 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 23 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 788 19 15 m_list init
R 789 19 16 m_list clean
R 790 19 17 m_list nullify
R 791 19 18 m_list get_indices
R 792 19 19 m_list test_indices
R 793 19 20 m_list nitem
R 794 19 21 m_list get
R 795 19 22 m_list identical
R 797 19 24 m_list copy
R 798 19 25 m_list exporttochar
R 799 19 26 m_list exporttostring
R 800 19 27 m_list charbuffersize
R 801 19 28 m_list append
R 802 19 29 m_list concatenate
R 803 19 30 m_list bcast
R 804 19 31 m_list send
R 805 19 32 m_list recv
R 806 19 33 m_list getsharedlistindices
R 812 19 39 m_list index
R 823 19 50 m_list allocated
R 988 19 22 m_attrvect init
R 989 19 23 m_attrvect clean
R 990 19 24 m_attrvect zero
R 991 19 25 m_attrvect lsize
R 992 19 26 m_attrvect niattr
R 993 19 27 m_attrvect nrattr
R 994 19 28 m_attrvect indexia
R 995 19 29 m_attrvect indexra
R 996 19 30 m_attrvect getilist
R 997 19 31 m_attrvect getrlist
R 998 19 32 m_attrvect exportilist
R 999 19 33 m_attrvect exportrlist
R 1000 19 34 m_attrvect exportilisttochar
R 1001 19 35 m_attrvect exportrlisttochar
R 1002 19 36 m_attrvect appendiattr
R 1003 19 37 m_attrvect appendrattr
R 1004 19 38 m_attrvect exportiattr
R 1005 19 39 m_attrvect exportrattr
R 1006 19 40 m_attrvect importiattr
R 1007 19 41 m_attrvect importrattr
R 1008 19 42 m_attrvect copy
R 1009 19 43 m_attrvect rcopy
R 1010 19 44 m_attrvect icopy
R 1011 19 45 m_attrvect sort
R 1012 19 46 m_attrvect permute
R 1013 19 47 m_attrvect unpermute
R 1014 19 48 m_attrvect sortpermute
R 1015 19 49 m_attrvect sharedattrindexlist
R 1283 25 3 m_generalgrid generalgrid
R 1284 5 4 m_generalgrid coordinate_list generalgrid
R 1285 5 5 m_generalgrid coordinate_sort_order generalgrid
R 1287 5 7 m_generalgrid descend generalgrid
R 1288 5 8 m_generalgrid descend$sd generalgrid
R 1289 5 9 m_generalgrid descend$p generalgrid
R 1290 5 10 m_generalgrid descend$o generalgrid
R 1292 5 12 m_generalgrid weight_list generalgrid
R 1293 5 13 m_generalgrid other_list generalgrid
R 1294 5 14 m_generalgrid index_list generalgrid
R 1295 5 15 m_generalgrid data generalgrid
R 1296 19 16 m_generalgrid init
R 1297 19 17 m_generalgrid initcartesian
R 1298 19 18 m_generalgrid initunstructured
R 1299 19 19 m_generalgrid clean
R 1300 19 20 m_generalgrid zero
R 1301 19 21 m_generalgrid dims
R 1302 19 22 m_generalgrid indexia
R 1303 19 23 m_generalgrid indexra
R 1304 19 24 m_generalgrid lsize
R 1305 19 25 m_generalgrid exportiattr
R 1306 19 26 m_generalgrid exportrattr
R 1307 19 27 m_generalgrid importiattr
R 1308 19 28 m_generalgrid importrattr
R 1309 19 29 m_generalgrid sort
R 1310 19 30 m_generalgrid permute
R 1311 19 31 m_generalgrid sortpermute
S 1521 19 0 0 0 8 1 582 7972 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 218 2 0 0 0 0 0 582 0 0 0 0 gather
O 1521 2 1527 1526
S 1522 19 0 0 0 8 1 582 7979 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 222 2 0 0 0 0 0 582 0 0 0 0 scatter
O 1522 2 1529 1528
S 1523 19 0 0 0 8 1 582 4824 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 224 1 0 0 0 0 0 582 0 0 0 0 bcast
O 1523 1 1530
S 1524 19 0 0 0 8 1 582 5561 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 226 1 0 0 0 0 0 582 0 0 0 0 send
O 1524 1 1531
S 1525 19 0 0 0 8 1 582 5566 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 228 1 0 0 0 0 0 582 0 0 0 0 recv
O 1525 1 1532
S 1526 27 0 0 0 8 1642 582 7987 10010 400000 A 0 0 0 0 0 0 249 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gm_gather_
Q 1526 1521 0
S 1527 27 0 0 0 8 1941 582 7998 10010 400000 A 0 0 0 0 0 0 302 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gsm_gather_
Q 1527 1521 0
S 1528 27 0 0 0 8 1949 582 8010 10010 400000 A 0 0 0 0 0 0 303 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gm_scatter_
Q 1528 1522 0
S 1529 27 0 0 0 8 1957 582 8022 10010 400000 A 0 0 0 0 0 0 304 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 gsm_scatter_
Q 1529 1522 0
S 1530 27 0 0 0 8 1965 582 4914 10010 400000 A 0 0 0 0 0 0 305 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 bcast_
Q 1530 1523 0
S 1531 27 0 0 0 8 1535 582 5795 10010 400000 A 0 0 0 0 0 0 229 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 send_
Q 1531 1524 0
S 1532 27 0 0 0 8 1541 582 5801 10010 400000 A 0 0 0 0 0 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 recv_
Q 1532 1525 0
S 1533 3 0 0 0 450 0 1 0 0 0 A 0 0 0 0 0 0 0 0 8035 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 23 4d 43 54 3a 3a 6d 5f 47 65 6e 65 72 61 6c 47 72 69 64 43 6f 6d 6d 73
S 1534 16 0 0 0 450 1 582 4980 14 440000 A 0 0 0 0 0 0 0 0 1533 497 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
S 1535 23 5 0 0 0 1540 582 5795 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 send_
S 1536 1 3 1 0 301 1 1535 7683 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iggrid
S 1537 1 3 1 0 6 1 1535 8059 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comp_id
S 1538 1 3 1 0 6 1 1535 5999 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tagbase
S 1539 1 3 2 0 6 1 1535 5987 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 status
S 1540 14 5 0 0 0 1 1535 5795 10 400000 A 0 0 0 0 0 0 0 423 4 0 0 0 0 0 0 0 0 0 0 0 0 85 0 582 0 0 0 0 send_
F 1540 4 1536 1537 1538 1539
S 1541 23 5 0 0 0 1546 582 5801 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 recv_
S 1542 1 3 2 0 301 1 1541 7676 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 oggrid
S 1543 1 3 1 0 6 1 1541 8059 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comp_id
S 1544 1 3 1 0 6 1 1541 5999 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tagbase
S 1545 1 3 2 0 6 1 1541 5987 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 status
S 1546 14 5 0 0 0 1 1541 5801 10 400000 A 0 0 0 0 0 0 0 428 4 0 0 0 0 0 0 0 0 0 0 0 0 336 0 582 0 0 0 0 recv_
F 1546 4 1542 1543 1544 1545
R 1550 25 1 m_globalmap globalmap
R 1551 5 2 m_globalmap comp_id globalmap
R 1552 5 3 m_globalmap gsize globalmap
R 1553 5 4 m_globalmap lsize globalmap
R 1555 5 6 m_globalmap counts globalmap
R 1556 5 7 m_globalmap counts$sd globalmap
R 1557 5 8 m_globalmap counts$p globalmap
R 1558 5 9 m_globalmap counts$o globalmap
R 1561 5 12 m_globalmap displs globalmap
R 1562 5 13 m_globalmap displs$sd globalmap
R 1563 5 14 m_globalmap displs$p globalmap
R 1564 5 15 m_globalmap displs$o globalmap
R 1566 19 17 m_globalmap gsize
R 1567 19 18 m_globalmap lsize
R 1568 19 19 m_globalmap init
R 1569 19 20 m_globalmap init_remote
R 1570 19 21 m_globalmap clean
R 1571 19 22 m_globalmap rank
R 1572 19 23 m_globalmap bounds
R 1573 19 24 m_globalmap comp_id
S 1642 23 5 0 0 0 1649 582 7987 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gm_gather_
S 1643 1 3 1 0 301 1 1642 8433 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ig
S 1644 1 3 2 0 301 1 1642 8436 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 og
S 1645 1 3 1 0 483 1 1642 8331 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 1646 1 3 1 0 6 1 1642 5086 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1647 1 3 1 0 6 1 1642 5091 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1648 1 3 2 0 6 1 1642 5096 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1649 14 5 0 0 0 1 1642 7987 10 400000 A 0 0 0 0 0 0 0 469 6 0 0 0 0 0 0 0 0 0 0 0 0 612 0 582 0 0 0 0 gm_gather_
F 1649 6 1643 1644 1645 1646 1647 1648
R 1652 25 1 m_globalsegmap globalsegmap
R 1653 19 2 m_globalsegmap init
R 1654 19 3 m_globalsegmap clean
R 1655 5 4 m_globalsegmap comp_id globalsegmap
R 1656 5 5 m_globalsegmap gsize globalsegmap
R 1657 19 6 m_globalsegmap globalstorage
R 1658 19 7 m_globalsegmap processstorage
R 1659 19 8 m_globalsegmap orderedpoints
R 1660 19 9 m_globalsegmap lsize
R 1661 5 10 m_globalsegmap ngseg globalsegmap
R 1662 19 11 m_globalsegmap nlseg
R 1663 19 12 m_globalsegmap max_nlseg
R 1664 19 13 m_globalsegmap active_pes
R 1665 19 14 m_globalsegmap pelocs
R 1666 19 15 m_globalsegmap haloed
R 1667 19 16 m_globalsegmap rank
R 1668 19 17 m_globalsegmap sort
R 1669 19 18 m_globalsegmap permute
R 1670 19 19 m_globalsegmap sortpermute
R 1671 19 20 m_globalsegmap increasing
R 1672 19 21 m_globalsegmap copy
R 1674 5 23 m_globalsegmap start globalsegmap
R 1675 5 24 m_globalsegmap start$sd globalsegmap
R 1676 5 25 m_globalsegmap start$p globalsegmap
R 1677 5 26 m_globalsegmap start$o globalsegmap
R 1680 5 29 m_globalsegmap length globalsegmap
R 1681 5 30 m_globalsegmap length$sd globalsegmap
R 1682 5 31 m_globalsegmap length$p globalsegmap
R 1683 5 32 m_globalsegmap length$o globalsegmap
R 1686 5 35 m_globalsegmap pe_loc globalsegmap
R 1687 5 36 m_globalsegmap pe_loc$sd globalsegmap
R 1688 5 37 m_globalsegmap pe_loc$p globalsegmap
R 1689 5 38 m_globalsegmap pe_loc$o globalsegmap
R 1698 19 47 m_globalsegmap comp_id
R 1700 19 49 m_globalsegmap gsize
R 1706 19 55 m_globalsegmap ngseg
S 1941 23 5 0 0 0 1948 582 7998 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsm_gather_
S 1942 1 3 1 0 301 1 1941 8433 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ig
S 1943 1 3 2 0 301 1 1941 8436 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 og
S 1944 1 3 1 0 601 1 1941 8963 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 1945 1 3 1 0 6 1 1941 5086 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1946 1 3 1 0 6 1 1941 5091 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1947 1 3 2 0 6 1 1941 5096 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1948 14 5 0 0 0 1 1941 7998 10 400000 A 0 0 0 0 0 0 0 585 6 0 0 0 0 0 0 0 0 0 0 0 0 729 0 582 0 0 0 0 gsm_gather_
F 1948 6 1942 1943 1944 1945 1946 1947
S 1949 23 5 0 0 0 1956 582 8010 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gm_scatter_
S 1950 1 3 1 0 301 1 1949 8433 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ig
S 1951 1 3 2 0 301 1 1949 8436 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 og
S 1952 1 3 1 0 452 1 1949 8331 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gmap
S 1953 1 3 1 0 6 1 1949 5086 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1954 1 3 1 0 6 1 1949 5091 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1955 1 3 2 0 6 1 1949 5096 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1956 14 5 0 0 0 1 1949 8010 10 400000 A 0 0 0 0 0 0 0 592 6 0 0 0 0 0 0 0 0 0 0 0 0 838 0 582 0 0 0 0 gm_scatter_
F 1956 6 1950 1951 1952 1953 1954 1955
S 1957 23 5 0 0 0 1964 582 8022 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsm_scatter_
S 1958 1 3 1 0 301 1 1957 8433 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ig
S 1959 1 3 2 0 301 1 1957 8436 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 og
S 1960 1 3 1 0 489 1 1957 8963 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gsmap
S 1961 1 3 1 0 6 1 1957 5086 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1962 1 3 1 0 6 1 1957 5091 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1963 1 3 2 0 6 1 1957 5096 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1964 14 5 0 0 0 1 1957 8022 10 400000 A 0 0 0 0 0 0 0 599 6 0 0 0 0 0 0 0 0 0 0 0 0 959 0 582 0 0 0 0 gsm_scatter_
F 1964 6 1958 1959 1960 1961 1962 1963
S 1965 23 5 0 0 0 1970 582 4914 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bcast_
S 1966 1 3 3 0 301 1 1965 9263 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iog
S 1967 1 3 1 0 6 1 1965 5086 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1968 1 3 1 0 6 1 1965 5091 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1969 1 3 2 0 6 1 1965 5096 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1970 14 5 0 0 0 1 1965 4914 10 400000 A 0 0 0 0 0 0 0 606 4 0 0 0 0 0 0 0 0 0 0 0 0 1074 0 582 0 0 0 0 bcast_
F 1970 4 1966 1967 1968 1969
S 1971 23 5 0 0 0 1976 582 9267 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 bcastgeneralgridheader_
S 1972 1 3 3 0 301 1 1971 9291 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ioggrid
S 1973 1 3 1 0 6 1 1971 5086 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 root
S 1974 1 3 1 0 6 1 1971 5091 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1975 1 3 2 0 6 1 1971 5096 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 stat
S 1976 14 5 0 0 0 1 1971 9267 10 400000 A 0 0 0 0 0 0 0 611 4 0 0 0 0 0 0 0 0 0 0 0 0 1192 0 582 0 0 0 0 bcastgeneralgridheader_
F 1976 4 1972 1973 1974 1975
S 1977 23 5 0 0 0 1980 582 9299 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 copygeneralgridheader_
S 1978 1 3 1 0 301 1 1977 7683 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iggrid
S 1979 1 3 2 0 301 1 1977 7676 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 oggrid
S 1980 14 5 0 0 0 1 1977 9299 10 400000 A 0 0 0 0 0 0 0 616 2 0 0 0 0 0 0 0 0 0 0 0 0 1426 0 582 0 0 0 0 copygeneralgridheader_
F 1980 2 1978 1979
A 134 2 0 0 0 6 772 0 0 0 134 0 0 0 0 0 0 0 0 0
A 497 2 0 0 0 450 1533 0 0 0 497 0 0 0 0 0 0 0 0 0
Z
Z
