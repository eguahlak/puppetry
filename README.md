# OBS!

Den aktive branch er pt. "develop"


# IoP - Dukketeaterkode

* Raspberry pie med wifi dongle
* Arduino Uno
* Adafruit neo strips

| Strip                     | Pixels  | Pin |
| ------------------------- |:-------:| ---:|
| *Clock*                   |     n/a |   7 |
| **B**aglys                |      27 |   6 |
| **M**ellemlys             |      27 |   5 |
| **F**orlys                |      27 |   4 |
| **P**roscenielys          |      23 |   3 | 
| **S**idelys (**L**,**R**) |      12 |   2 |

## Protokol mellem Raspberry pie og Arduino

Raspberry pie og Arduino kommunikerer serielt via USB.
Første (og forhåbentlig ikke sidste) version af protokollen er som følger:
```
[BMFPSLRX][0-9a-f]{9}\n
```
`Lff0000` tænder fuldt grønt lys i venstre side, `S008000` tænder halvt rødt lys i begge sider og `X0000ff`
tænder blåt lys på hele teatret. Rækkefølgen er grøn-rød-blå af uvisse årsager.
Ideen er at protokollen skal understøtte farvesætning af de individuelle pixels.
