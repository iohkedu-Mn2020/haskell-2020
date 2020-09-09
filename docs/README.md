# Haskell and Cryptocurrencies. Mongolia 2020

![Haskell Logo](https://upload.wikimedia.org/wikipedia/commons/1/1c/Haskell-Logo.svg)

- Lectures: Mondays, Wednesdays and Fridays
- **Instructors:**
  - [Lars Brünjes](https://iohk.io/en/team/lars-brunjes)
  - [Andres Löh](http://www.well-typed.com/people/andres/)
- **TA**
  - [Alejandro García](https://iohk.io/en/team/alejandro-garcia)

## Course Description

Haskell is a high-level, purely functional programming language with a
strong static type system and elegant mathematical underpinnings. It is
being increasingly used in industry by organizations such as Facebook,
AT\&T, and NASA, along with several financial firms. We will explore the
joys of functional programming, using Haskell as a vehicle. In the first
part of the course, you will learn to use Haskell to easily and
conveniently write practical programs. The last couple of weeks will
consist of several special topics that explore advanced features of
Haskell. Evaluation will be based on class participation, weekly
programming assignments, and an open-ended final project.

## Getting the Software

You will need `ghc` (the Haskell compiler; at least version 8.6.5, preferably
version 8.10.1), `cabal` (a build tool for Haskell; at least version 2.4.1,
preferably version 3.2), and a suitable development editor to edit source files
with (e.g. Visual Studio Code, or (neo)vim).

Instructions on how to install the necessary software are available [here](https://github.com/zfoh/haskell-simple-install/blob/master/README.md),
separated by platform (Linux, MacOS, Windows).

These instructions also include information on how to set up more advanced IDE-like
editor support for Visual Studio Code, in the form of `ghcide`. While this will be
helpful in the long run, it is not mandatory, and if you experience problems setting
this up, don't worry for now.

## How to get help

### Students portal

Is the first source of information. Here you will find
lecture, notes, exercises, etc.
Visit it in: [haskell-mongolia-2020](https://github.com/iohkedu/haskell-mongolia-2020)

### Content Questions

Questions like:

- How do I install Haskell on Windows? [video](https://drive.google.com/file/d/1vxSk3Qeb_SyCNuwvs6jpQRmM2BNrVRro/view?usp=sharing)
- Which text editor should I use?
- What is a Cabal Package?
  It should go in: [Stackoverflow IohkEdu](https://stackoverflow.com/c/iohkedu/questions)
  On stackoverflow team students are _encouraged_ to answer each others questions.

[Alejandro Garcia](alejandro.garcia@iohk.io) will monitor the forum closely
so that we don't let questions go unanswered.

### Defects

If you found a defect in one of the sample programs.
Or maybe a typo in one of the slides we will really appreciate it
if you could post it as an issue on GitHub Itself.
Report defecs in: [haskell-mongolia-2020/issues](https://github.com/iohkedu/haskell-mongolia-2020/issues)

### Administrative Questions

Related to the Lab in Mongolia should be directed to:
Dugerdorj Davaadorj <dugerdorj.davaadorj@iohk.io>

## Class

| Date       | Title                                    | Video       | Chat       | Assignment                   |
| ---------- | ---------------------------------------- | ----------- | ---------- | ---------------------------- |
| 20.07.2020 | [Welcome][1]                             | [video][1]  | [chat][2]  |                              |
|            | [Introduction to Cryptocurrencies][4]    |             |            |                              |
|            | [An Overview of Haskell][5]              |             |            | [Introduction][6]            |
| 22.07.2020 | [Datatypes & Functions][7]               | [video][8]  | [chat][9]  | [Datatypes & Functions][10]  |
| 24.07.2020 | [Polymorphism & Type Classes][11]        | [video][12] | [chat][13] |                              |
| 27.07.2020 | [Higher-Order Functions][14]             | [video][15] | [chat][16] | [Higher-Order Functions][17] |
|            | [Packaging & Tools][18]                  |             |            |                              |
| 29.07.2020 | [IO][19]                                 | [video][20] | [chat][21] | [IO][22]                     |
| 31.07.2020 | [Testing][23]                            | [video][24] | [chat][25] |                              |
| 03.08.2020 | [Abstraction Patterns][26]               | [video][27] | [chat][28] |                              |
| 05.08.2020 | [Concurrency][29]                        | [video][30] | [chat][31] |                              |
| 07.08.2020 | [Network Servers][32]                    | [video][33] | [chat][34] |                              |
| 10.08.2020 | [Parser Combinators][35]                 | [video][36] | [chat][37] |                              |
| 12.08.2020 | [More on Parsing][38]                    | [video][39] | [chat][40] |                              |
| 14.08.2020 | [Data Structures][41]                    | [video][42] | [chat][43] |                              |
| 17.08.2020 | [More on Data Structures][44]            | [video][45] | [chat][46] |                              |
| 19.08.2020 | [Optics][47]                             | [video][48] | [chat][49] |                              |
| 21.08.2020 | [More Optics][50]                        | [video][51] | [chat][52] |                              |
| 24.08.2020 | [More on Monads][53]                     | [video][54] | [chat][55] |                              |
| 26.08.2020 | [Free Monads][56]                        | [video][57] | [chat][58] |                              |
| 28.08.2020 | [Streaming][59]                          | [video][60] | [chat][61] |                              |
| 31.08.2020 | [Embedded Domain-Specific Languages][62] | [video][63] | [chat][64] |                              |
| 02.09.2020 | Marlowe I                                | [video][65] | [chat][66] |                              |
| 04.09.2020 | Marlowe II                               | [video][67] | [chat][68] |                              |
| 07.09.2020 | Marlowe III                              | [video][69] | [chat][70] |                              |

[1]:   ../lectures/00-welcome.pdf
[2]:   https://drive.google.com/file/d/1u0xNcuoi9cLTFMenfEbNRXqe0S5sI-nj/view?usp=sharing
[3]:   https://drive.google.com/file/d/1OVoowel76o5tedNLYxxCyPN6qopGD6wK/view?usp=sharing
[4]:   ../lectures/01-intro-cryptocurrencies.pdf
[5]:   ../lectures/02-overview-haskell.pdf
[6]:   https://classroom.github.com/a/ZFu9YQF5
[7]:  ../lectures/03-datatypes-functions.pdf
[8]:   https://drive.google.com/file/d/127LklblBCX-2VsHKy3cHWeXWyhFr5lma/view?usp=sharing
[9]:   https://drive.google.com/file/d/127LklblBCX-2VsHKy3cHWeXWyhFr5lma/view?usp=sharing
[10]:  https://classroom.github.com/a/YjmNAnkP
[11]:  ../lectures/04-polymorphism-type-classes.pdf
[12]:  https://drive.google.com/file/d/11MVm_fiqpaEFavEOSXKQ6zBuSRRp5MCE/view?usp=sharing
[13]:  https://drive.google.com/file/d/1E3fRkTX5-NhUHP1YFC01Dtt2s8AYCrjL/view?usp=sharing
[14]:  ../lectures/05-higher-order-functions.pdf
[15]:  https://drive.google.com/file/d/1CzKVzIwuNVvtbZk30VOKaEb2paCxAD_r/view?usp=sharing
[16]:  https://drive.google.com/file/d/1KaWzbrCgNBXL9gUGsgYO198orrHhYtUN/view?usp=sharing
[17]:  https://classroom.github.com/a/8_VyrI5G
[18]:  ../lectures/06-packaging-and-tools.pdf
[19]:  ../lectures/07-io.pdf
[20]:  https://drive.google.com/file/d/1WuTl_Z_xvmZJv0AD0vlQYs63ja68gFfz/view?usp=sharing
[21]:  https://drive.google.com/file/d/11H2lVjCV60GXJVSt3zl5XX66QXDGHIhc/view?usp=sharing
[22]:  https://classroom.github.com/a/_eDITQUZ
[23]:  ../lectures/08-testing.pdf
[24]:  https://drive.google.com/file/d/1UXqmBjNMPuJxHbsAMTDnihdnw6IZRt73/view?usp=sharing
[25]:  https://drive.google.com/file/d/1ueiGfD0fjbLJh6nHcg33Rtb_zLAZsRyV/view?usp=sharing
[26]:  ../lectures/09-abstraction-patterns.pdf
[27]:  https://drive.google.com/file/d/1pfgRzEeioNNWM__AvQ_FPczdy08-ON0W/view?usp=sharing
[28]:  https://drive.google.com/file/d/1NS35BfBXdpFv1MSW6sP1lWPOMEqafeUr/view?usp=sharing
[29]:  ../lectures/10-concurrency.pdf
[30]:  https://drive.google.com/file/d/1r36dUeCLBZdU69CoM-OFIggicXVMEcwk/view?usp=sharing
[31]:  https://drive.google.com/file/d/1tj4964piFXiNVhFvAhJEt8VmksFKH7VR/view?usp=sharing
[32]:  ../lectures/11-servers.pdf
[33]:  https://drive.google.com/file/d/1_Po9UjEOO9TW0VUjxXgQNd1R9x-TtjB8/view?usp=sharing
[34]:  https://drive.google.com/file/d/1didVeqSJ9Z_UeX-mCvV8RS4FYsFQTRQu/view?usp=sharing
[35]:  ../lectures/12-parser-combinators.pdf
[36]:  https://drive.google.com/file/d/14PIL_2ZhROoR1QBkH5851FEP7tv0UG_z/view?usp=sharing
[37]:  https://drive.google.com/file/d/14FTpgRSN5Y5qqPXekADKG1eWGv31P7hZ/view?usp=sharing
[38]:  ../lectures/13-more-parsing.pdf
[39]:  https://drive.google.com/file/d/1dZoaL0NQHpvNqni27kD4o0cW_uUMnumK/view?usp=sharing
[40]:  https://drive.google.com/file/d/1Rm6gx2qCCTXZMKNqEtXhWnfPhsUYUXSw/view?usp=sharing
[41]:  ../lectures/14-data-structures.pdf
[42]:  https://drive.google.com/file/d/1yfLx1ZZax_2ftcDJSrUnqHJavFaVmKDW/view?usp=sharing
[43]:  https://drive.google.com/file/d/1GUkcECr4NIXuLE8JC78bMABDvlxhjBJU/view?usp=sharing
[44]:  ../lectures/15-more-data-structures.pdf
[45]:  https://drive.google.com/file/d/18jm_OhzdcBFNtdtZPqMNKOPJG8Licl6H/view?usp=sharing
[46]:  https://drive.google.com/file/d/1-uUgA2bwLHf2Xg3ekGFgE5ITFaHN8ce0/view?usp=sharing
[47]:  ../lectures/16-optics.pdf
[48]:  https://drive.google.com/file/d/17zdvG7jZGhUdS3jJYcAeOwmaVucGcIUq/view?usp=sharing
[49]:  https://drive.google.com/file/d/1pVBmKqLSbmXDh0kwyqpPnQY9W9s9eFMQ/view?usp=sharing
[50]:  ../lectures/17-more-optics.pdf
[51]:  https://drive.google.com/file/d/1diCeBDVNCxl97kFAF7S8V4jhqLHucLB0/view?usp=sharing
[52]:  https://drive.google.com/file/d/1gbVNoOFhuQCUtDr79gswScrYNjiONUpb/view?usp=sharing
[53]:  ../lectures/18-more-monads.pdf
[54]:  https://drive.google.com/file/d/1WEbuxxFYZvThn5TOSaIgvfT7peSwBa6k/view?usp=sharing
[55]:  https://drive.google.com/file/d/1hR91DeUj9FYqhFDZvko9nclWEgDVqr4O/view?usp=sharing
[56]:  ../lectures/19-free-monads.pdf
[57]:  https://drive.google.com/file/d/1hOEQu8l-3Lv4vr1sTv3gvJBUpumQEu5Y/view?usp=sharing
[58]:  https://drive.google.com/file/d/1HUy0_jwxDmo_iueRoB3gKzmNf82-ckxh/view?usp=sharing
[59]:  ../lectures/20-streaming.pdf
[60]:  https://drive.google.com/file/d/1TO0TVHJZWrwMvzGCRagE7STB3WsJniuD/view?usp=sharing
[61]:  https://drive.google.com/file/d/1yVpAiy4jQ6F7ZfiDPPG5LDrZWHMui-Oq/view?usp=sharing
[62]:  ../lectures/21-dsls.pdf
[63]:  https://drive.google.com/file/d/1VeBZum65R3R0BWSMaG8VS8yUW3epge9u/view?usp=sharing
[64]:  https://drive.google.com/file/d/17wJGLVhkVDR3FXP0Vc4TL07e_yjdQtLs/view?usp=sharing
[65]:  https://drive.google.com/file/d/1sAGeMxvz8u73gZ0IEznRnfMF1pTiv82Y/view?usp=sharing
[66]:  https://drive.google.com/file/d/1lpUHD1DNIXt9Y2FNl3OLKjRQ1JMbvSTF/view?usp=sharing
[67]:  https://drive.google.com/file/d/1DK2k23rlQ-Y_Lc-AGqu1tkvMELSX_R3T/view?usp=sharing
[68]:  https://drive.google.com/file/d/1z4JDttyvrTPHC2M7LPUemcXItJAhPLOf/view?usp=sharing
[69]:  https://drive.google.com/file/d/1wFjlLNJ14h6my8-1kw2sThmHMGJfN2Ef/view?usp=sharing
[70]:  https://drive.google.com/file/d/17H6-LQElMJstZQp3wM998Zz1ckMoxdm4/view?usp=sharing

### Assignments

Homework are due on Fridays before the lecture, one week after they have been handed out.
All homework submission is via GitHub classroom.
If you have questions, check this video:
[How to submit your homework on GitHub classroom](https://youtu.be/MSe8xIEiulc)

- [w01](https://classroom.github.com/g/GZPFmDbA), due 7-August-2020, 12:30 MNG
- [w02](https://classroom.github.com/g/ZSww0ngM), due 14-August-2020, 12:30 MNG
- [w03](https://classroom.github.com/g/Wc4HWmsI), due 21-August-2020, 12:30 MNG
- [w04](https://classroom.github.com/g/3zLh1EOf), due 28-August-2020, 12:30 MNG
- [w05](https://classroom.github.com/g/sO3WMGuz), due 4-September-2020, 12:30 MNG
- [w06](https://classroom.github.com/g/JTzSeMXf), due 11-September-2020, 12:30 MNG
- [p1](https://classroom.github.com/g/pQ5wHPtj), due 21-August-2020, 12:30 MNG

### Tests

Every two weeks there will be a little exam.
Don't worry, it's the same technique as the homeworks,
the only difference is that here the time windows is shorter and
that you will work alone, not in a team.

| Date       | Time              | Test
| ---------- | ----------------- | --------------------------------------------- |
| 29.07.2020 | 12:30 - 14:30 MNG | [t1](https://classroom.github.com/a/C4Juj31P) |
| 12.08.2020 | 12:30 - 14:30 MNG | [t2](https://classroom.github.com/a/dHzLBhZ_) |
| 26.08.2020 | 12:30 - 14:30 MNG | [t3](https://classroom.github.com/a/AqhOUQ2e) |
| 09.09.2020 | 12:30 - 14:30 MNG | [t4](https://classroom.github.com/a/kNUSk7mP) |

### Examples and Q&A

| Date       | Video        | Chat        |
| ---------- | ------------ | ----------- |
| 18.08.2020 | [video][200] | [chat][201] |
| 20.08.2020 | [video][202] | [chat][203] |
| 25.08.2020 | [video][204] | [chat][205] |
| 27.08.2020 | [video][206] | [chat][207] |
| 01.09.2020 | [video][208] | [chat][209] |
| 03.09.2020 | [video][210] | [chat][211] |
| 08.09.2020 | [video][212] | [chat][213] |

[200]: https://drive.google.com/file/d/1_dVRHFZgZhQE27HhDVRLq_QdvJAP1yK0/view?usp=sharing
[201]: https://drive.google.com/file/d/1Wx-vzhmE4XiaocV2FrV5VtloG8xniDP8/view?usp=sharing
[202]: https://drive.google.com/file/d/1TAWPyPOLN1AdOQggiCzbyCQYnrOswrhD/view?usp=sharing
[203]: https://drive.google.com/file/d/1rDZmNo0wcnys_kljQYMb_ObNzP_R9wXo/view?usp=sharing
[204]: https://drive.google.com/file/d/1B3A_HhnJ7YsooKq5dcbdqm7n6QbO4myY/view?usp=sharing
[205]: https://drive.google.com/file/d/1Vlc5BbO92EjNqvWmpMviZL5ARVI8kSu_/view?usp=sharing
[206]: https://drive.google.com/file/d/1t7sMpQsnId7eY_4Bvdg664Zh-9me9Xli/view?usp=sharing
[207]: https://drive.google.com/file/d/1NB6CmN6zVAlOI1XBtmUbxM3mc1GC83pT/view?usp=sharing
[208]: https://drive.google.com/file/d/1ABo-iMUsXeMXS9fRzCyAlGkgoAvapSDK/view?usp=sharing
[209]: https://drive.google.com/file/d/1YtJsnBTEYxWRwQ9LRLOkVg5VfbgM6Cl5/view?usp=sharing
[210]: https://drive.google.com/file/d/14j7Z1QraMofS4IzX-X9a7a7J-KliHaRx/view?usp=sharing
[211]: https://drive.google.com/file/d/1nJMgP3m8cP1pwCWgx1-C_jnPFYxJlsvh/view?usp=sharing
[212]: https://drive.google.com/file/d/1W7odvqAiYLkbaz8fyrnl1BRTRjVrsJ3V/view?usp=sharing
[213]: https://drive.google.com/file/d/1HlP3uZ-hEp9z6X6DT2YgOzCKwo370yHu/view?usp=sharing

### Bibliography

We have read most of this books and they provide good information for beginners.

1. Lipovača M. [Learn You A Haskell For Great Good!](http://learnyouahaskell.com/). San Francisco: No Starch Press; 2012.

>  Beginner Friendly book with lots of examples.

2. O'Sullivan B, Stewart D, Goerzen J. [Real World Haskell](http://book.realworldhaskell.org/). Farnham: O'Reilly; 2009.

>  A lot of real world techniques, just a little bit outdated at this point.

3. Bird R. [Algorithm Design With Haskell](https://www.amazon.com/Algorithm-Design-Haskell-Richard-Bird-ebook/dp/B08BKXJ1N3/ref=tmm_kin_swatch_0?_encoding=UTF8&qid=1597814133&sr=8-1). Cambridge University Press; 2020.

>  Learn how far you can get with immutable data, to implement almost all standard algorithms.

4. Thibaut C. [Texas Hold'em: The Little Haskeller](https://leanpub.com/texasholdem-tlh).leanpub.com; 2019.

>  A long article, written in the Quesntion and Answer format. Like the Little Schemer.

5. Penner C. [Optics By Example: Functional Lenses In Haskell](https://leanpub.com/optics-by-example). leanpub.com; 2020.

>  The only book on optics.
