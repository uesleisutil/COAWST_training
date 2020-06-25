V26 m_spatialintegral
21 m_SpatialIntegral.F90 S582 0
03/26/2019  11:41:37
use m_list private
use m_string private
use m_generalgrid private
use m_attrvect private
use m_list private
use m_string private
use m_generalgrid private
use m_attrvect private
enduse
D 56 18 12
D 194 24 989 624 988 7
D 301 24 1302 1712 1301 7
D 450 24 989 624 988 7
D 456 24 1302 1712 1301 7
S 582 24 0 0 0 6 1 0 4658 10015 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 63 0 0 0 0 0 0 m_spatialintegral
S 583 19 0 0 0 8 1 582 4676 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 0 0 0 0 0 582 0 0 0 0 spatialintegral
O 583 1 591
S 584 19 0 0 0 8 1 582 4692 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 1 0 0 0 0 0 582 0 0 0 0 spatialaverage
O 584 1 592
S 585 19 0 0 0 6 1 582 4707 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 1 0 0 0 0 0 582 0 0 0 0 maskedspatialintegral
O 585 1 593
S 586 19 0 0 0 6 1 582 4729 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 1 0 0 0 0 0 582 0 0 0 0 maskedspatialaverage
O 586 1 594
S 587 19 0 0 0 8 1 582 4750 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 1 0 0 0 0 0 582 0 0 0 0 pairedspatialintegrals
O 587 1 595
S 588 19 0 0 0 8 1 582 4773 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 12 1 0 0 0 0 0 582 0 0 0 0 pairedspatialaverages
O 588 1 596
S 589 19 0 0 0 8 1 582 4795 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 14 1 0 0 0 0 0 582 0 0 0 0 pairedmaskedspatialintegrals
O 589 1 597
S 590 19 0 0 0 8 1 582 4824 4000 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16 1 0 0 0 0 0 582 0 0 0 0 pairedmaskedspatialaverages
O 590 1 598
S 591 27 0 0 0 8 1539 582 4852 10010 400000 A 0 0 0 0 0 0 231 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 spatialintegralrattrgg_
Q 591 583 0
S 592 27 0 0 0 8 1547 582 4876 10010 400000 A 0 0 0 0 0 0 232 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 spatialaveragerattrgg_
Q 592 584 0
S 593 27 0 0 0 6 1554 582 4899 10010 400000 A 0 0 0 0 0 0 233 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 maskedspatialintegralrattrgg_
Q 593 585 0
S 594 27 0 0 0 6 1566 582 4929 10010 400000 A 0 0 0 0 0 0 234 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 maskedspatialaveragerattrgg_
Q 594 586 0
S 595 27 0 0 0 8 1576 582 4958 10010 400000 A 0 0 0 0 0 0 235 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 pairedspatialintegralrattrgg_
Q 595 587 0
S 596 27 0 0 0 8 1588 582 4988 10010 400000 A 0 0 0 0 0 0 236 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 pairedspatialaveragerattrgg_
Q 596 588 0
S 597 27 0 0 0 8 1599 582 5017 10010 400000 A 0 0 0 0 0 0 237 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 pairedmaskedintegralrattrgg_
Q 597 589 0
S 598 27 0 0 0 8 1616 582 5046 10010 400000 A 0 0 0 0 0 0 238 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 pairedmaskedaveragerattrgg_
Q 598 590 0
S 599 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 0 0 0 0 0 22 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 600 3 0 0 0 56 0 1 0 0 0 A 0 0 0 0 0 0 0 0 5074 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 22 4d 43 54 3a 3a 6d 5f 53 70 61 74 69 61 6c 49 6e 74 65 67 72 61 6c
S 601 16 0 0 0 56 1 582 5097 14 440000 A 0 0 0 0 0 0 0 0 600 13 0 0 0 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 myname
R 618 19 8 m_string tochar
R 619 19 9 m_string string_init
R 620 19 10 m_string init
R 621 19 11 m_string string_clean
R 622 19 12 m_string clean
R 623 19 13 m_string string_len
R 624 19 14 m_string string_bcast
R 625 19 15 m_string bcast
R 626 19 16 m_string string_mci
R 627 19 17 m_string string_mco
R 628 19 18 m_string ptr_chars
R 629 19 19 m_string char
R 637 19 27 m_string len
R 792 19 15 m_list init
R 793 19 16 m_list clean
R 794 19 17 m_list nullify
R 795 19 18 m_list get_indices
R 796 19 19 m_list test_indices
R 797 19 20 m_list nitem
R 798 19 21 m_list get
R 799 19 22 m_list identical
R 801 19 24 m_list copy
R 802 19 25 m_list exporttochar
R 803 19 26 m_list exporttostring
R 804 19 27 m_list charbuffersize
R 805 19 28 m_list append
R 806 19 29 m_list concatenate
R 807 19 30 m_list bcast
R 808 19 31 m_list send
R 809 19 32 m_list recv
R 810 19 33 m_list getsharedlistindices
R 816 19 39 m_list index
R 827 19 50 m_list allocated
R 988 25 5 m_attrvect attrvect
R 989 5 6 m_attrvect ilist attrvect
R 990 5 7 m_attrvect rlist attrvect
R 993 5 10 m_attrvect iattr attrvect
R 994 5 11 m_attrvect iattr$sd attrvect
R 995 5 12 m_attrvect iattr$p attrvect
R 996 5 13 m_attrvect iattr$o attrvect
R 1000 5 17 m_attrvect rattr attrvect
R 1001 5 18 m_attrvect rattr$sd attrvect
R 1002 5 19 m_attrvect rattr$p attrvect
R 1003 5 20 m_attrvect rattr$o attrvect
R 1005 19 22 m_attrvect init
R 1006 19 23 m_attrvect clean
R 1007 19 24 m_attrvect zero
R 1008 19 25 m_attrvect lsize
R 1009 19 26 m_attrvect niattr
R 1010 19 27 m_attrvect nrattr
R 1011 19 28 m_attrvect indexia
R 1012 19 29 m_attrvect indexra
R 1013 19 30 m_attrvect getilist
R 1014 19 31 m_attrvect getrlist
R 1015 19 32 m_attrvect exportilist
R 1016 19 33 m_attrvect exportrlist
R 1017 19 34 m_attrvect exportilisttochar
R 1018 19 35 m_attrvect exportrlisttochar
R 1019 19 36 m_attrvect appendiattr
R 1020 19 37 m_attrvect appendrattr
R 1021 19 38 m_attrvect exportiattr
R 1022 19 39 m_attrvect exportrattr
R 1023 19 40 m_attrvect importiattr
R 1024 19 41 m_attrvect importrattr
R 1025 19 42 m_attrvect copy
R 1026 19 43 m_attrvect rcopy
R 1027 19 44 m_attrvect icopy
R 1028 19 45 m_attrvect sort
R 1029 19 46 m_attrvect permute
R 1030 19 47 m_attrvect unpermute
R 1031 19 48 m_attrvect sortpermute
R 1032 19 49 m_attrvect sharedattrindexlist
R 1301 25 3 m_generalgrid generalgrid
R 1302 5 4 m_generalgrid coordinate_list generalgrid
R 1303 5 5 m_generalgrid coordinate_sort_order generalgrid
R 1305 5 7 m_generalgrid descend generalgrid
R 1306 5 8 m_generalgrid descend$sd generalgrid
R 1307 5 9 m_generalgrid descend$p generalgrid
R 1308 5 10 m_generalgrid descend$o generalgrid
R 1310 5 12 m_generalgrid weight_list generalgrid
R 1311 5 13 m_generalgrid other_list generalgrid
R 1312 5 14 m_generalgrid index_list generalgrid
R 1313 5 15 m_generalgrid data generalgrid
R 1314 19 16 m_generalgrid init
R 1315 19 17 m_generalgrid initcartesian
R 1316 19 18 m_generalgrid initunstructured
R 1317 19 19 m_generalgrid clean
R 1318 19 20 m_generalgrid zero
R 1319 19 21 m_generalgrid dims
R 1320 19 22 m_generalgrid indexia
R 1321 19 23 m_generalgrid indexra
R 1322 19 24 m_generalgrid lsize
R 1323 19 25 m_generalgrid exportiattr
R 1324 19 26 m_generalgrid exportrattr
R 1325 19 27 m_generalgrid importiattr
R 1326 19 28 m_generalgrid importrattr
R 1327 19 29 m_generalgrid sort
R 1328 19 30 m_generalgrid permute
R 1329 19 31 m_generalgrid sortpermute
S 1539 23 5 0 0 0 1546 582 4852 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 spatialintegralrattrgg_
S 1540 1 3 1 0 450 1 1539 8392 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav
S 1541 1 3 2 0 450 1 1539 8397 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav
S 1542 1 3 1 0 456 1 1539 7965 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid
S 1543 1 3 1 0 28 1 1539 8403 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 weighttag
S 1544 1 3 1 0 16 1 1539 8413 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sumweights
S 1545 1 3 1 0 6 1 1539 5497 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1546 14 5 0 0 0 1 1539 4852 10 400000 A 0 0 0 0 0 0 0 423 6 0 0 0 0 0 0 0 0 0 0 0 0 174 0 582 0 0 0 0 spatialintegralrattrgg_
F 1546 6 1540 1541 1542 1543 1544 1545
S 1547 23 5 0 0 0 1553 582 4876 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 spatialaveragerattrgg_
S 1548 1 3 1 0 194 1 1547 8392 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav
S 1549 1 3 2 0 194 1 1547 8397 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav
S 1550 1 3 1 0 301 1 1547 7965 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid
S 1551 1 3 1 0 28 1 1547 8403 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 weighttag
S 1552 1 3 1 0 6 1 1547 5497 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1553 14 5 0 0 0 1 1547 4876 10 400000 A 0 0 0 0 0 0 0 430 5 0 0 0 0 0 0 0 0 0 0 0 0 299 0 582 0 0 0 0 spatialaveragerattrgg_
F 1553 5 1548 1549 1550 1551 1552
S 1554 23 5 0 0 0 1565 582 4899 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 maskedspatialintegralrattrgg_
S 1555 1 3 1 0 194 1 1554 8392 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav
S 1556 1 3 2 0 194 1 1554 8397 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav
S 1557 1 3 1 0 301 1 1554 7965 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid
S 1558 1 3 1 0 28 1 1554 8424 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 spatialweighttag
S 1559 1 3 1 0 28 1 1554 8441 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 imasktags
S 1560 1 3 1 0 28 1 1554 8451 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rmasktags
S 1561 1 3 1 0 16 1 1554 8461 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 usefastmethod
S 1562 1 3 1 0 16 1 1554 8413 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sumweights
S 1563 1 3 1 0 28 1 1554 8475 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 weightsumtag
S 1564 1 3 1 0 6 1 1554 5497 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1565 14 5 0 0 0 1 1554 4899 10 400000 A 0 0 0 0 0 0 0 436 10 0 0 0 0 0 0 0 0 0 0 0 0 440 0 582 0 0 0 0 maskedspatialintegralrattrgg_
F 1565 10 1555 1556 1557 1558 1559 1560 1561 1562 1563 1564
S 1566 23 5 0 0 0 1575 582 4929 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 maskedspatialaveragerattrgg_
S 1567 1 3 1 0 194 1 1566 8392 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav
S 1568 1 3 2 0 194 1 1566 8397 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav
S 1569 1 3 1 0 301 1 1566 7965 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid
S 1570 1 3 1 0 28 1 1566 8424 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 spatialweighttag
S 1571 1 3 1 0 28 1 1566 8441 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 imasktags
S 1572 1 3 1 0 28 1 1566 8451 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rmasktags
S 1573 1 3 1 0 16 1 1566 8461 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 usefastmethod
S 1574 1 3 1 0 6 1 1566 5497 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1575 14 5 0 0 0 1 1566 4929 10 400000 A 0 0 0 0 0 0 0 447 8 0 0 0 0 0 0 0 0 0 0 0 0 962 0 582 0 0 0 0 maskedspatialaveragerattrgg_
F 1575 8 1567 1568 1569 1570 1571 1572 1573 1574
S 1576 23 5 0 0 0 1587 582 4958 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pairedspatialintegralrattrgg_
S 1577 1 3 1 0 194 1 1576 8488 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav1
S 1578 1 3 2 0 194 1 1576 8494 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav1
S 1579 1 3 1 0 301 1 1576 8501 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid1
S 1580 1 3 1 0 28 1 1576 8508 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 weighttag1
S 1581 1 3 1 0 194 1 1576 8519 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav2
S 1582 1 3 2 0 194 1 1576 8525 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav2
S 1583 1 3 1 0 301 1 1576 8532 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid2
S 1584 1 3 1 0 28 1 1576 8539 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 weighttag2
S 1585 1 3 1 0 16 1 1576 8413 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sumweights
S 1586 1 3 1 0 6 1 1576 5497 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1587 14 5 0 0 0 1 1576 4958 10 400000 A 0 0 0 0 0 0 0 456 10 0 0 0 0 0 0 0 0 0 0 0 0 1184 0 582 0 0 0 0 pairedspatialintegralrattrgg_
F 1587 10 1577 1578 1579 1580 1581 1582 1583 1584 1585 1586
S 1588 23 5 0 0 0 1598 582 4988 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pairedspatialaveragerattrgg_
S 1589 1 3 1 0 194 1 1588 8488 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav1
S 1590 1 3 2 0 194 1 1588 8494 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav1
S 1591 1 3 1 0 301 1 1588 8501 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid1
S 1592 1 3 1 0 28 1 1588 8508 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 weighttag1
S 1593 1 3 1 0 194 1 1588 8519 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav2
S 1594 1 3 2 0 194 1 1588 8525 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav2
S 1595 1 3 1 0 301 1 1588 8532 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid2
S 1596 1 3 1 0 28 1 1588 8539 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 weighttag2
S 1597 1 3 1 0 6 1 1588 5497 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1598 14 5 0 0 0 1 1588 4988 10 400000 A 0 0 0 0 0 0 0 467 9 0 0 0 0 0 0 0 0 0 0 0 0 1334 0 582 0 0 0 0 pairedspatialaveragerattrgg_
F 1598 9 1589 1590 1591 1592 1593 1594 1595 1596 1597
S 1599 23 5 0 0 0 1615 582 5017 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pairedmaskedintegralrattrgg_
S 1600 1 3 1 0 194 1 1599 8488 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav1
S 1601 1 3 2 0 194 1 1599 8494 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav1
S 1602 1 3 1 0 301 1 1599 8501 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid1
S 1603 1 3 1 0 28 1 1599 8550 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 spatialweighttag1
S 1604 1 3 1 0 28 1 1599 8568 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rmasktags1
S 1605 1 3 1 0 28 1 1599 8579 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 imasktags1
S 1606 1 3 1 0 194 1 1599 8519 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav2
S 1607 1 3 2 0 194 1 1599 8525 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav2
S 1608 1 3 1 0 301 1 1599 8532 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid2
S 1609 1 3 1 0 28 1 1599 8590 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 spatialweighttag2
S 1610 1 3 1 0 28 1 1599 8608 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rmasktags2
S 1611 1 3 1 0 28 1 1599 8619 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 imasktags2
S 1612 1 3 1 0 16 1 1599 8461 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 usefastmethod
S 1613 1 3 1 0 16 1 1599 8413 80000014 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sumweights
S 1614 1 3 1 0 6 1 1599 5497 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1615 14 5 0 0 0 1 1599 5017 10 400000 A 0 0 0 0 0 0 0 477 15 0 0 0 0 0 0 0 0 0 0 0 0 1505 0 582 0 0 0 0 pairedmaskedintegralrattrgg_
F 1615 15 1600 1601 1602 1603 1604 1605 1606 1607 1608 1609 1610 1611 1612 1613 1614
S 1616 23 5 0 0 0 1631 582 5046 10 0 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pairedmaskedaveragerattrgg_
S 1617 1 3 1 0 194 1 1616 8488 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav1
S 1618 1 3 2 0 194 1 1616 8494 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav1
S 1619 1 3 1 0 301 1 1616 8501 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid1
S 1620 1 3 1 0 28 1 1616 8550 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 spatialweighttag1
S 1621 1 3 1 0 28 1 1616 8568 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rmasktags1
S 1622 1 3 1 0 28 1 1616 8579 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 imasktags1
S 1623 1 3 1 0 194 1 1616 8519 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 inav2
S 1624 1 3 2 0 194 1 1616 8525 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 outav2
S 1625 1 3 1 0 301 1 1616 8532 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ggrid2
S 1626 1 3 1 0 28 1 1616 8590 14 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 spatialweighttag2
S 1627 1 3 1 0 28 1 1616 8608 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rmasktags2
S 1628 1 3 1 0 28 1 1616 8619 80000014 43000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 imasktags2
S 1629 1 3 1 0 16 1 1616 8461 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 usefastmethod
S 1630 1 3 1 0 6 1 1616 5497 14 3000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comm
S 1631 14 5 0 0 0 1 1616 5046 10 400000 A 0 0 0 0 0 0 0 493 14 0 0 0 0 0 0 0 0 0 0 0 0 1766 0 582 0 0 0 0 pairedmaskedaveragerattrgg_
F 1631 14 1617 1618 1619 1620 1621 1622 1623 1624 1625 1626 1627 1628 1629 1630
A 12 2 0 0 0 6 599 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 2 0 0 0 56 600 0 0 0 13 0 0 0 0 0 0 0 0 0
Z
Z
