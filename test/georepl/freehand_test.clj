(ns georepl.freehand_test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.freehand :refer :all]))


(def arc-test1  '((349 482 1462) (349 482 1461) (347 483 1461) (346 485 1407) (348 485 1406) (352 483 1389) (357 481 1378) (366 476 1378)
                  (375 471 1378) (386 466 1354) (397 459 1343) (408 452 1343) (421 443 1343) (432 434 1323) (443 423 1323) (450 413 1307)
                  (456 402 1307) (463 391 1291) (470 378 1291) (472 367 1273) (474 355 1272) (474 344 1259) (474 334 1259) (470 323 1258)
                  (465 313 1238) (461 301 1238) (456 292 1220) (452 283 1220) (447 275 1205) (443 267 1205) (440 264 1189) (438 261 1189)
                  (437 259 1173) (435 258 1173) (428 256 1156) (421 254 1156) (410 254 1142) (399 254 1141) (384 254 1140) (369 254 1118)
                  (354 256 1105) (337 258 1105) (323 260 1090) (308 265 1089) (295 269 1089) (279 274 1069) (269 279 1068) (261 283 1052)
                  (255 287 1043) (249 292 1043) (244 295 1043) (242 297 1017) (240 299 1017) (240 303 967)))

(def result-arc1_1 '((349 482 1462) (348 485 1406) (386 466 1354) (443 423 1323) (472 367 1273) (465 313 1238)
                     (443 267 1205) (428 256 1156) (369 254 1118) (295 269 1089) (249 292 1043) (240 303 967)))

(deftest dash-velocity-test
  (testing "dash-velocity"
    (is (= 0.0 (dash-velocity [])))
    (is (= 0.0 (dash-velocity [[714 283 1405]])))
    (is (= 0.0 (dash-velocity [[714 283 1405][714 283 1405]])))
    (is (= 0.0 (dash-velocity [[714 283 1633][714 283 1405]])))
    (is (= 4.0 (dash-velocity '([244 577 3601] [244 579 3601] [244 581 3600]))))
    (is (math/equals? 12.369317054748535 (dash-velocity '([549 594 4380] [546 606 4380]))))))



(deftest distribute-points-test
  (testing "distribute-points"
    (is (= result-arc1_1 (distribute-points arc-test1)))))



(def arc-test2 '([274 150] 45
                ((252 113 9202) (252 113 9201) (248 114 9186) (249 114 9164) (250 114 9154) (252 116 9154) (258 118 9154) (276 125 9118) (287 132 9118) (297 136 9103) (311 136 9103)
                 (315 142 9102) (319 146 9083) (323 151 9069) (326 153 9069) (328 156 9069) (328 158 9050) (328 162 9050) (328 164 9035) (326 167 9024) (324 173 9024) (323 176 9023)
                 (322 179 9023) (320 182 8986) (319 184 8986) (317 186 8970) (316 187 8970) (313 188 8951) (309 188 8950) (303 188 8933) (293 186 8932) (283 186 8932) (275 182 8916)
                 (267 178 8903) (261 173 8903) (255 169 8887) (249 163 8887) (242 157 8869) (237 151 8869) (233 146 8854) (229 142 8854) (226 138 8853) (226 135 8833) (223 133 8833)
                 (222 131 8819) (222 129 8818) (222 127 8803) (222 125 8802) (222 123 8784) (222 121 8783) (222 119 8768) (222 118 8768) (222 117 8751) (223 112 8702))))

(def result-arc2_1 '(4.175953522779958 3.7819491186606355))

(deftest arc-segment-test
  (testing "arc-segment"
    (is (= result-arc2_1 (arc-segment (first arc-test2) (second arc-test2) (first (drop 2 arc-test2)))))))


(deftest average-test
  (testing "average"
    (is (= 0 (average [])))
    (is (= 42 (average [42])))
    (is (= 3 (average [1 2 3 4 5])))
    ))

(deftest bias-test
  (testing "bias"
    (is (math/equals? 0.1 (bias 2.0 [2.5 1.5 2.2 1.8 2.1 1.9])))
    ))



(def arc1 '((218 136 4870) (218 136 4821) (218 137 4821) (219 139 4820) (219 140 4797) (219 142 4787) (219 143 4787) (218 145 4787)
            (218 146 4763) (218 147 4763) (217 151 4753) (212 155 4753) (208 159 4732) (203 164 4732) (199 168 4716) (191 172 4716)
            (184 177 4716) (174 181 4692) (165 183 4691) (156 187 4678) (149 189 4678) (140 191 4664) (132 191 4663) (125 191 4642)
            (120 191 4642) (115 191 4631) (109 189 4631) (105 187 4611) (101 185 4611) (97 182 4595) (95 181 4594) (93 175 4579)
            (90 169 4579) (88 164 4564) (86 158 4564) (84 152 4548) (81 146 4548) (79 141 4530) (79 133 4530) (78 126 4529) (78 118 4512)
            (78 112 4512) (79 107 4495) (79 102 4481) (82 95 4481) (83 92 4480) (85 89 4462) (88 86 4461) (92 84 4442) (96 81 4442)
            (100 79 4426) (104 76 4425) (109 74 4425) (115 73 4407) (123 71 4406) (131 71 4406) (141 71 4381) (150 71 4381) (161 73 4380)
            (167 74 4357) (171 75 4356) (177 77 4257)))

(def arc2 '((376 147 9586) (376 147 9586) (376 148 9575) (376 149 9574) (377 149 9574) (377 150 9541) (378 151 9541) (378 153 9517)
            (379 154 9516) (380 155 9505) (380 158 9504) (381 159 9487) (381 161 9486) (382 163 9468) (383 164 9468) (384 166 9468)
            (386 169 9446) (388 171 9445) (390 173 9432) (392 175 9432) (393 177 9415) (395 178 9414) (397 180 9396) (399 181 9396)
            (401 182 9378) (404 183 9378) (405 184 9362) (406 184 9362) (408 184 9347) (409 184 9346) (410 184 9346) (411 184 9328)
            (413 183 9328) (415 183 9312) (416 182 9298) (418 182 9298) (420 181 9297) (422 180 9277) (423 180 9277) (426 178 9260)
            (427 178 9250) (429 176 9249) (430 176 9248) (431 175 9226) (431 174 9226) (432 173 9216) (433 170 9216) (434 169 9194)
            (434 167 9194) (435 165 9194) (436 163 9176) (437 161 9166) (439 158 9165) (440 156 9165) (440 153 9143) (441 150 9132)
            (441 147 9132) (441 144 9131) (441 140 9112) (441 137 9111) (441 133 9094) (441 130 9094) (441 127 9080) (441 125 9080)
            (441 122 9059) (441 120 9059) (441 118 9045) (440 116 9045) (440 114 9028) (440 112 9028) (439 110 9010) (439 108 9010)
            (438 105 8995) (438 103 8995) (437 101 8994) (436 98 8976) (435 96 8962) (433 93 8962) (433 92 8961) (432 90 8944) (430 88 8944)
            (430 86 8877) (429 85 8859) (428 85 8859) (426 85 8843) (423 84 8843) (419 83 8829) (416 83 8828) (412 83 8828) (409 83 8808)
            (405 83 8793) (403 83 8793) (400 83 8792) (398 83 8775) (397 83 8774) (395 84 8758) (394 84 8757) (393 84 8742) (393 85 8742)
            (392 85 8724) (391 86 8709) (390 87 8709) (389 87 8708) (389 88 8689) (388 89 8674) (387 90 8658) (386 91 8658) (386 92 8657)
            (386 93 8640) (386 94 8640) (384 98 8507)))

(def arc3 '((163 376 16160) (163 376 16160) (163 375 16142) (164 375 16078) (163 374 16078) (163 373 16056) (163 372 16055) (163 371 16043)
            (163 370 16042) (163 369 16024) (162 367 16023) (162 366 16008) (162 365 16008) (162 363 15992) (162 362 15992) (161 360 15973)
            (161 358 15973) (160 357 15958) (160 356 15958) (159 355 15941) (158 354 15941) (158 353 15924) (156 351 15923) (156 350 15908)
            (155 350 15908) (155 349 15891) (154 349 15891) (153 348 15874) (151 348 15874) (150 347 15858) (147 347 15857) (145 347 15839)
            (142 347 15839) (140 347 15823) (139 347 15823) (137 347 15807) (136 347 15806) (135 348 15788) (134 348 15788) (132 349 15773)
            (130 350 15773) (128 351 15755) (126 352 15754) (123 354 15739) (121 356 15739) (120 357 15739) (117 359 15722) (116 360 15722)
            (114 362 15705) (113 363 15704) (112 365 15687) (110 367 15686) (109 368 15671) (106 373 15505)))

(def arc4 '((474 372 18103) (474 372 18018) (473 372 18018) (472 372 17993) (471 372 17993) (467 372 17993) (463 372 17992) (455 372 17968)
            (448 372 17967) (440 370 17950) (435 370 17949) (429 369 17933) (423 369 17932) (418 366 17916) (412 365 17915) (408 363 17900)
            (402 359 17899) (397 355 17882) (393 350 17881) (390 347 17866) (389 343 17866) (387 340 17865) (386 336 17849) (384 333 17848)
            (383 331 17832) (383 329 17831) (385 325 17748)))

(def line1 '((618 138 12719) (618 138 12532) (617 138 12517) (616 138 12503) (614 138 12503) (611 138 12485) (609 138 12484) (605 137 12468)
             (603 137 12468) (601 137 12451) (598 136 12451) (596 136 12450) (593 135 12432) (590 134 12418) (587 134 12417) (585 133 12417)
             (582 132 12401) (580 131 12400) (579 130 12383) (577 128 12383) (575 127 12368) (573 126 12367) (572 124 12347) (570 122 12346)
             (567 118 12216)))

(def circle1 '((334 63 12147) (334 63 11972) (335 63 11972) (339 63 11971) (345 65 11971) (351 65 11945) (358 66 11935) (367 68 11935) (376 68 11935)
               (385 70 11911) (394 70 11911) (403 70 11898) (408 70 11898) (413 70 11881) (418 71 11880) (423 71 11861) (427 71 11861) (430 72 11849)
               (434 72 11849) (436 73 11849) (439 74 11823) (442 75 11823) (444 77 11811) (445 77 11811) (448 79 11790) (449 80 11789) (450 82 11779)
               (451 83 11778) (451 84 11758) (452 85 11758) (453 86 11758) (453 87 11739) (453 88 11729) (453 90 11729) (453 91 11729) (453 93 11705)
               (453 95 11695) (453 96 11694) (452 98 11694) (452 100 11673) (452 102 11673) (451 104 11656) (450 105 11646) (449 107 11646)
               (445 109 11646) (439 111 11621) (432 113 11621) (425 117 11610) (418 121 11610) (408 125 11589) (401 129 11589) (392 132 11575)
               (385 133 11575) (380 135 11557) (376 135 11557) (374 135 11544) (371 135 11543) (370 135 11521) (369 135 11511) (368 135 11511)
               (367 134 11510) (366 133 11489) (365 132 11480) (364 130 11480) (363 128 11479) (362 126 11454) (362 123 11453) (363 121 11443)
               (363 119 11442) (364 116 11442) (365 113 11418) (366 110 11407) (367 107 11407) (367 104 11406) (368 101 11387) (368 97 11387)
               (370 94 11370) (372 90 11370) (376 85 11353) (380 81 11352) (387 77 11332) (393 75 11325) (401 72 11325) (406 70 11324) (414 68 11301)
               (419 66 11300) (425 65 11300) (430 63 11282) (435 62 11201)))


(def circle2 '((352 510 15138) (352 510 15121) (352 511 15106) (352 512 15086) (352 513 15075) (353 514 14987) (354 514 14971) (355 513 14953)
               (359 511 14941) (363 510 14941) (369 505 14940) (374 501 14917) (382 496 14917) (390 492 14907) (398 487 14907) (406 483 14907)
               (413 479 14883) (421 474 14873) (428 470 14872) (437 466 14872) (445 465 14849) (452 461 14838) (457 457 14837) (463 453 14820)
               (466 451 14820) (469 449 14820) (472 447 14803) (474 445 14789) (477 442 14788) (478 441 14788) (480 438 14770) (480 436 14769)
               (480 434 14751) (480 432 14742) (480 430 14742) (480 428 14742) (480 424 14741) (480 423 14702) (479 420 14702) (479 419 14701)
               (479 417 14680) (478 416 14680) (478 415 14663) (477 414 14649) (476 413 14648) (475 412 14634) (472 411 14634) (467 409 14612)
               (462 409 14612) (456 409 14612) (451 409 14595) (447 409 14585) (444 410 14585) (440 410 14584) (438 411 14561) (434 412 14549)
               (432 414 14548) (428 415 14534) (426 417 14533) (425 418 14533) (424 420 14515) (423 422 14515) (423 423 14499) (423 425 14498)
               (423 428 14482) (423 429 14481) (423 433 14465) (423 437 14465) (423 441 14448) (423 445 14447) (423 449 14429) (425 455 14429)
               (427 460 14416) (429 466 14415) (431 471 14414) (433 477 14395) (435 483 14381) (438 488 14381) (441 494 14380) (444 499 14363)
               (447 504 14362) (449 507 14346) (450 511 14346) (451 515 14330) (453 518 14329) (454 520 14312) (455 522 14311) (457 524 14295)
               (456 529 14228)))


(def arc5 '((419 629 24057) (419 629 23956) (419 628 23955) (419 627 23940) (419 626 23939) (419 625 23926) (418 624 23925) (418 622 23904)
            (417 620 23891) (416 619 23890) (416 618 23873) (416 617 23873) (415 617 23873) (413 616 23855) (411 614 23842) (409 612 23841)
            (407 611 23819) (406 610 23819) (406 609 23807) (405 609 23807) (405 608 23773) (404 608 23754) (402 608 23753) (400 608 23740)
            (399 608 23740) (397 608 23724) (396 610 23724) (395 611 23708) (394 612 23707) (393 613 23707) (393 614 23688) (392 615 23674)
            (392 616 23674) (391 618 23674) (390 620 23653) (389 622 23653) (389 626 23640) (389 629 23640) (390 633 23621) (390 637 23620)
            (391 641 23603) (391 644 23603) (392 647 23586) (392 651 23585) (393 652 23570) (394 655 23570) (394 656 23555) (395 657 23555)
            (396 658 23540) (401 660 23540) (407 660 23540) (413 661 23518) (422 663 23505) (430 665 23505) (440 667 23490) (449 667 23490)
            (457 665 23489) (460 664 23471) (463 664 23471) (466 662 23456) (469 659 23456) (471 655 23437) (473 651 23437) (475 645 23422)
            (477 639 23422) (479 634 23404) (481 628 23404) (481 621 23387) (481 615 23387) (479 607 23373) (479 600 23372) (477 594 23372)
            (474 587 23351) (473 580 23339) (469 573 23339) (468 569 23322) (466 565 23321) (465 562 23306) (464 560 23305) (463 558 23305)
            (463 556 23287) (461 554 23287) (460 552 23272) (458 551 23272) (455 547 23240)))

(def arc6 '((684 616 25733) (684 616 25721) (683 617 25705) (683 618 25692) (682 618 25609) (681 618 25609) (680 618 25593) (680 617 25593)
            (679 617 25576) (677 616 25576) (676 616 25558) (674 614 25558) (673 613 25542) (671 611 25542) (669 609 25527) (669 608 25526)
            (668 606 25507) (667 605 25506) (665 604 25490) (665 602 25490) (663 600 25477) (663 598 25476) (663 597 25476) (663 595 25457)
            (663 594 25457) (663 593 25441) (663 592 25426) (664 590 25425) (664 589 25410) (665 588 25410) (665 587 25409) (666 586 25392)
            (667 583 25392) (668 582 25374) (668 580 25373) (669 579 25357) (670 578 25357) (671 577 25343) (675 576 25343) (680 574 25322)
            (687 574 25322) (696 574 25309) (705 573 25308) (713 571 25293) (717 570 25292) (722 570 25292) (724 569 25272) (728 569 25259)
            (730 569 25259) (731 569 25242) (733 569 25242) (734 569 25241) (737 570 25225) (739 570 25224) (741 571 25206) (744 572 25205)
            (746 574 25189) (747 576 25189) (749 578 25175) (752 582 25174) (754 586 25158) (756 591 25157) (759 597 25140) (761 603 25140)
            (764 609 25124) (764 616 25124) (764 622 25106) (764 628 25105) (764 634 25090) (762 640 25090) (760 646 25076) (756 653 25075)
            (751 659 25075) (746 666 25056) (742 672 25056) (737 679 25041) (732 688 25040) (723 697 25022) (714 702 25021) (703 707 25008)
            (690 709 25008) (675 711 24992) (662 711 24992) (650 711 24972) (637 709 24971) (625 707 24958) (610 705 24957) (600 702 24939)
            (590 700 24938) (582 696 24924) (574 691 24923) (569 688 24908) (567 686 24907) (565 685 24891) (565 683 24890) (565 682 24890)
            (566 678 24840)))

(def circle3 '((525 458 32420) (525 458 32337) (526 458 32254) (527 458 32254) (529 458 32235) (530 458 32235) (532 459 32219) (534 459 32219)
               (536 460 32205) (538 460 32204) (539 461 32204) (541 461 32182) (541 462 32170) (543 463 32170) (544 464 32170) (544 465 32150)
               (545 465 32150) (545 466 32133) (545 467 32123) (546 467 32123) (546 468 32100) (546 469 32086) (547 469 32068) (547 470 32068)
               (547 471 32040) (547 472 32040) (547 473 32010) (547 474 32009) (547 475 31993) (547 476 31978) (546 477 31959) (546 478 31944)
               (545 479 31944) (545 480 31928) (545 481 31928) (544 482 31912) (544 483 31911) (544 484 31896) (544 485 31877) (543 485 31861)
               (543 486 31861) (542 486 31827) (541 487 31811) (540 487 31811) (539 488 31797) (538 489 31796) (537 489 31796) (537 490 31776)
               (536 490 31763) (535 490 31763) (534 490 31728) (533 490 31712) (532 489 31711) (531 489 31694) (530 488 31694) (528 488 31678)
               (527 486 31677) (525 485 31662) (523 484 31661) (521 483 31644) (520 482 31644) (518 480 31629) (517 479 31629) (516 477 31629)
               (515 477 31595) (515 476 31595) (514 476 31578) (514 475 31577) (513 474 31561) (513 472 31546) (512 472 31546) (512 471 31545)
               (512 470 31527) (511 468 31527) (511 467 31511) (511 466 31495) (511 465 31495) (511 464 31478) (511 463 31477) (512 462 31460)
               (513 460 31460) (513 459 31444) (513 458 31444) (513 457 31427) (513 455 31427) (513 454 31411) (513 453 31410) (513 452 31410)
               (513 451 31393) (513 450 31379) (513 449 31379) (513 448 31379) (513 447 31359) (514 447 31359) (514 446 31344) (514 445 31344)
               (515 445 31326) (515 444 31326) (515 443 31295) (515 442 31260) (516 442 31225) (517 442 31211) (519 442 31210) (521 442 31210)
               (523 442 31192) (526 442 31192) (528 442 31176) (531 442 31175) (535 442 31161) (538 443 31160) (542 443 31139) (545 443 31139)
               (548 444 31127) (551 445 31126) (553 446 31110) (555 446 31109) (557 447 31091) (560 447 31091) (561 448 31077) (563 449 31076)
               (565 450 31076) (566 451 31054) (567 452 31042) (568 454 31042) (569 455 31042) (569 456 31023) (571 458 31022) (571 459 31008)
               (572 460 31008) (572 462 30992) (572 464 30992) (572 466 30970) (572 468 30970) (572 470 30957) (572 472 30957) (572 474 30942)
               (572 476 30942) (572 479 30942) (571 480 30920) (570 482 30920) (570 484 30903) (569 486 30903) (568 488 30886) (567 490 30875)
               (567 492 30874) (567 493 30858) (566 495 30857) (566 496 30857) (565 498 30839) (564 500 30839) (564 502 30822) (563 503 30821)
               (562 504 30806) (561 506 30805) (561 507 30789) (560 508 30788) (559 509 30773) (557 510 30773) (555 510 30755) (550 510 30755)
               (544 510 30740) (538 510 30740) (533 510 30723) (528 508 30722) (520 506 30706) (512 502 30705) (503 499 30691) (495 497 30691)
               (488 494 30690) (481 492 30670) (475 491 30658) (470 487 30657) (465 485 30657) (461 483 30637) (458 481 30637) (455 479 30621)
               (451 475 30546)))

(def sinus1 '((552 216 4383) (552 216 4383) (556 225 4382) (559 233 4382) (561 244 4357) (564 252 4346) (566 260 4346) (566 266 4328) (566 273 4328)
              (564 280 4314) (562 287 4314) (555 296 4313) (548 303 4290) (539 310 4289) (530 317 4273) (519 326 4264) (506 335 4264) (494 343 4264)
              (481 350 4245) (466 357 4245) (452 364 4245) (440 370 4221) (426 377 4212) (415 378 4212) (404 380 4212) (396 380 4188) (389 380 4178)
              (384 380 4178) (380 380 4177) (376 379 4154) (372 377 4146) (368 375 4146) (364 373 4145) (360 369 4121) (358 365 4120) (356 357 4108)
              (352 352 4108) (350 346 4094) (349 340 4093) (347 335 4093) (345 329 4070) (345 324 4059) (344 318 4059) (344 312 4059) (342 307 4037)
              (342 301 4024) (341 295 4023) (341 290 4023) (339 282 4007) (337 275 4006) (337 267 3990) (336 260 3990) (336 255 3970) (333 249 3970)
              (332 245 3958) (329 241 3957) (327 239 3936) (325 237 3936) (321 234 3925) (317 232 3925) (310 230 3905) (304 228 3905) (295 225 3889)
              (283 223 3889) (271 221 3876) (259 219 3875) (247 217 3875) (231 217 3853) (220 217 3843) (207 217 3843) (200 217 3843) (193 217 3818)
              (187 217 3808) (182 219 3807) (177 219 3807) (173 220 3787) (169 222 3787) (165 224 3771) (160 228 3761) (156 232 3761) (154 236 3760)
              (152 240 3760) (148 246 3734) (145 252 3721) (143 258 3720) (141 266 3720) (138 274 3702) (136 282 3687) (134 292 3687) (132 301 3687)
              (130 309 3667) (129 316 3667) (129 321 3650) (127 325 3650) (126 329 3633) (125 332 3632) (124 336 3622) (123 337 3621) (121 339 3621)
              (120 341 3597) (120 342 3597) (119 344 3580) (117 348 3498)))

(def zwo1 '((626 618 8066) (626 618 7933) (625 618 7932) (616 618 7912) (601 618 7911) (582 616 7899) (561 616 7898) (534 616 7884) (497 616 7884)
            (462 614 7883) (421 614 7861) (386 614 7848) (353 614 7848) (314 612 7834) (279 607 7833) (258 605 7833) (240 605 7811) (227 605 7800)
            (222 605 7800) (217 605 7799) (214 605 7778) (210 605 7778) (207 604 7766) (204 604 7766) (202 604 7744) (200 604 7744) (196 603 7727)
            (195 602 7727) (193 602 7716) (192 601 7617) (194 599 7617) (198 596 7616) (207 594 7597) (216 594 7597) (227 591 7582) (242 589 7581)
            (257 587 7562) (276 587 7562) (291 587 7548) (314 587 7548) (333 587 7529) (352 587 7529) (369 587 7514) (388 584 7514) (409 582 7499)
            (422 577 7498) (439 568 7480) (454 559 7480) (469 548 7463) (482 539 7463) (497 528 7448) (508 519 7447) (519 509 7447) (529 499 7431)
            (534 493 7430) (538 489 7415) (539 486 7414) (540 483 7394) (540 481 7394) (540 480 7382) (540 479 7381) (540 477 7363) (533 474 7362)
            (522 472 7347) (507 469 7347) (492 467 7331) (471 464 7330) (450 461 7312) (425 458 7312) (404 455 7297) (383 453 7297) (360 450 7279)
            (341 450 7279) (325 450 7263) (311 450 7263) (301 450 7247) (296 450 7247) (293 450 7246) (289 450 7227) (284 450 7196)))

(def V4eck '((369 92 16144) (369 92 16144) (368 92 16144) (368 93 16011) (369 93 15998) (370 93 15982) (370 94 15961) (371 95 15961) (372 95 15950)
             (373 96 15950) (374 97 15930) (375 97 15930) (376 98 15915) (376 99 15915) (377 99 15896) (377 100 15895) (378 100 15881) (379 100 15867)
             (380 101 15867) (382 101 15866) (383 102 15846) (385 103 15835) (386 103 15835) (389 105 15809) (390 105 15809) (392 106 15795)
             (394 106 15642) (395 107 15641) (396 107 15627) (400 108 15626) (404 108 15610) (410 108 15610) (418 108 15609) (426 108 15589)
             (436 108 15576) (443 108 15576) (450 108 15563) (453 109 15563) (455 109 15394) (456 109 15394) (457 109 15373) (459 109 15363)
             (461 109 15362) (463 109 15362) (465 109 15341) (468 109 15341) (470 109 15326) (472 109 15325) (475 109 15308) (477 109 15298)
             (479 109 15297) (482 109 15297) (484 109 15297) (486 109 15269) (489 109 15269) (492 109 15253) (495 109 15253) (499 108 15236)
             (502 108 15235) (505 107 15217) (508 106 15216) (511 105 15206) (513 104 15205) (515 103 15185) (517 103 15185) (518 102 14906)
             (518 103 14905) (517 103 14884) (517 104 14883) (516 104 14869) (515 106 14869) (514 107 14855) (514 108 14854) (512 109 14833)
             (512 110 14833) (511 111 14822) (511 112 14822) (510 113 14801) (509 114 14788) (508 115 14787) (507 117 14774) (505 118 14774)
             (504 120 14773) (502 124 14749) (500 125 14749) (498 129 14733) (496 133 14733) (492 137 14713) (488 141 14712) (484 147 14701)
             (480 153 14701) (476 158 14680) (470 164 14679) (465 170 14665) (459 175 14665) (453 181 14651) (449 185 14650) (446 187 14650)
             (443 190 14628) (441 193 14617) (439 195 14617) (437 198 14616) (437 199 14597) (436 199 14597) (435 199 14378) (434 199 14345)
             (434 198 14345) (433 198 14328) (433 197 14312) (432 197 14312) (432 196 14295) (431 196 14295) (430 196 14282) (429 196 14281)
             (429 195 14267) (427 194 14266) (426 193 14266) (425 191 14244) (421 189 14233) (417 187 14232) (412 186 14232) (405 184 14213)
             (396 181 14212) (387 178 14198) (379 176 14198) (368 174 14179) (357 172 14171) (344 170 14170) (331 168 14170) (318 166 14147)
             (304 163 14147) (291 161 14146) (275 158 14124) (263 156 14114) (251 152 14113) (244 148 14113) (236 145 14094) (230 142 14079)
             (227 140 14079) (225 139 14079) (224 138 14062) (225 137 14030) (225 136 14029) (226 136 13994) (227 135 13980) (228 135 13943)
             (228 134 13930) (229 134 13896) (230 134 13875) (230 133 13875) (231 133 13862) (231 132 13861) (233 132 13846) (234 132 13845)
             (235 131 13826) (237 130 13826) (238 130 13813) (240 129 13812) (245 127 13792) (246 126 13792) (249 125 13779) (250 124 13779)
             (252 123 13763) (254 123 13762) (257 122 13762) (262 120 13741) (268 118 13729) (274 113 13729) (283 109 13728) (293 104 13711)
             (303 100 13696) (314 95 13695) (324 91 13695) (333 87 13675) (338 83 13675) (342 80 13657) (345 78 13657) (349 74 13445)))






(deftest analyze-freehand-shape-test
  (testing "analyze-freehand-shape"
    (is (= :arc (:type (first (analyze-freehand-shape arc-test1)))))
    (is (= :arc (:type (first (analyze-freehand-shape arc1)))))
    (is (= :arc (:type (first (analyze-freehand-shape arc2)))))
    (is (= :contour (:type (first (analyze-freehand-shape line1)))))
    (is (= :arc (:type (first (analyze-freehand-shape arc3)))))
    (is (= :arc (:type (first (analyze-freehand-shape arc4)))))
    (is (= :circle (:type (first (analyze-freehand-shape circle1)))))
    (is (= :circle (:type (first (analyze-freehand-shape circle2)))))
    (is (= :arc (:type (first (analyze-freehand-shape arc5)))))
    (is (= :arc (:type (first (analyze-freehand-shape arc6)))))
    (is (= :circle (:type (first (analyze-freehand-shape circle3)))))
    (is (= :contour (:type (first (analyze-freehand-shape sinus1)))))
    (is (= :contour (:type (first (analyze-freehand-shape zwo1)))))
    (is (= :contour (:type (first (analyze-freehand-shape V4eck)))))
    ))

