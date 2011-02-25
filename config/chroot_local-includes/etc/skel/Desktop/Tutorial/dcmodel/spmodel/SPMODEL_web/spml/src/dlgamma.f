!TITLE  Gamma function produced by Dr. Ooura, RIMS, Kyoto Univ.
!
!HISTORY 2009/02/06  S.Takehiro: 
!        downloaded from http://www.kurims.kyoto-u.ac.jp/~ooura/gamerf.tar.gz
!
! log Gamma function in double precision
!
      function dlgamma(x)
      implicit real*8 (a - h, o - z)
      dimension a(0 : 21), b(0 : 97), c(0 : 64), d(0 : 6)
      parameter (pi = 3.141592653589793238d0)
      data (a(i), i = 0, 10) / 
     &    0.00009967270908702825d0, -0.00019831672170162227d0, 
     &    -0.00117085315349625822d0, 0.00722012810948319552d0, 
     &    -0.00962213009367802970d0, -0.04219772092994235254d0, 
     &    0.16653861065243609743d0, -0.04200263501129018037d0, 
     &    -0.65587807152061930091d0, 0.57721566490153514421d0, 
     &    0.99999999999999999764d0 / 
      data (a(i), i = 11, 21) / 
     &    0.00004672097259011420d0, -0.00006812300803992063d0, 
     &    -0.00132531159076610073d0, 0.00733521178107202770d0, 
     &    -0.00968095666383935949d0, -0.04217642811873540280d0, 
     &    0.16653313644244428256d0, -0.04200165481709274859d0, 
     &    -0.65587818792782740945d0, 0.57721567315209190522d0, 
     &    0.99999999973565236061d0 / 
      data (b(i), i = 0, 13) / 
     &    -0.00000000004587497028d0, 0.00000000019023633960d0, 
     &    0.00000000086377323367d0, 0.00000001155136788610d0, 
     &    -0.00000002556403058605d0, -0.00000015236723372486d0, 
     &    -0.00000316805106385740d0, 0.00000122903704923381d0, 
     &    0.00002334372474572637d0, 0.00111544038088797696d0, 
     &    0.00344717051723468982d0, 0.03198287045148788384d0, 
     &    -0.32705333652955399526d0, 0.40120442440953927615d0 / 
      data (b(i), i = 14, 27) / 
     &    -0.00000000005184290387d0, -0.00000000083355121068d0, 
     &    -0.00000000256167239813d0, 0.00000001455875381397d0, 
     &    0.00000013512178394703d0, 0.00000029898826810905d0, 
     &    -0.00000358107254612779d0, -0.00002445260816156224d0, 
     &    -0.00004417127762011821d0, 0.00112859455189416567d0, 
     &    0.00804694454346728197d0, 0.04919775747126691372d0, 
     &    -0.24818372840948854178d0, 0.11071780856646862561d0 / 
      data (b(i), i = 28, 41) / 
     &    0.00000000030279161576d0, 0.00000000160742167357d0, 
     &    -0.00000000405596009522d0, -0.00000005089259920266d0, 
     &    -0.00000002029496209743d0, 0.00000135130272477793d0, 
     &    0.00000391430041115376d0, -0.00002871505678061895d0, 
     &    -0.00023052137536922035d0, 0.00045534656385400747d0, 
     &    0.01153444585593040046d0, 0.07924014651650476036d0, 
     &    -0.12152192626936502982d0, -0.07916438300260539592d0 / 
      data (b(i), i = 42, 55) / 
     &    -0.00000000050919149580d0, -0.00000000115274986907d0, 
     &    0.00000001237873512188d0, 0.00000002937383549209d0, 
     &    -0.00000030621450667958d0, -0.00000077409414949954d0, 
     &    0.00000816753874325579d0, 0.00002412433382517375d0, 
     &    -0.00026061217606063700d0, -0.00091000087658659231d0, 
     &    0.01068093850598380797d0, 0.11395654404408482305d0, 
     &    0.07209569059984075595d0, -0.10971041451764266684d0 / 
      data (b(i), i = 56, 69) / 
     &    0.00000000040119897187d0, -0.00000000013224526679d0, 
     &    -0.00000001002723190355d0, 0.00000002569249716518d0, 
     &    0.00000020336011868466d0, -0.00000118097682726060d0, 
     &    -0.00000300660303810663d0, 0.00004402212897757763d0, 
     &    -0.00001462405876235375d0, -0.00164873795596001280d0, 
     &    0.00513927520866443706d0, 0.13843580753590579416d0, 
     &    0.32730190978254056722d0, 0.08588339725978624973d0 / 
      data (b(i), i = 70, 83) / 
     &    -0.00000000015413428348d0, 0.00000000064905779353d0, 
     &    0.00000000160702811151d0, -0.00000002655645793815d0, 
     &    0.00000007619544277956d0, 0.00000047604380765353d0, 
     &    -0.00000490748870866195d0, 0.00000821513040821212d0, 
     &    0.00014804944070262948d0, -0.00122152255762163238d0, 
     &    -0.00087425289205498532d0, 0.14438703699657968310d0, 
     &    0.61315889733595543766d0, 0.55513708159976477557d0 / 
      data (b(i), i = 84, 97) / 
     &    0.00000000001049740243d0, -0.00000000025832017855d0, 
     &    0.00000000139591845075d0, -0.00000000021177278325d0, 
     &    -0.00000005082950464905d0, 0.00000037801785193343d0, 
     &    -0.00000073982266659145d0, -0.00001088918441519888d0, 
     &    0.00012491810452478905d0, -0.00049171790705139895d0, 
     &    -0.00425707089448266460d0, 0.13595080378472757216d0, 
     &    0.89518356003149514744d0, 1.31073912535196238583d0 / 
      data (c(i), i = 0, 12) / 
     &    0.0000000116333640008d0, -0.0000000833156123568d0, 
     &    0.0000003832869977018d0, -0.0000015814047847688d0, 
     &    0.0000065010672324100d0, -0.0000274514060128677d0, 
     &    0.0001209015360925566d0, -0.0005666333178228163d0, 
     &    0.0029294103665559733d0, -0.0180340086069185819d0, 
     &    0.1651788780501166204d0, 1.1031566406452431944d0, 
     &    1.2009736023470742248d0 / 
      data (c(i), i = 13, 25) / 
     &    0.0000000013842760642d0, -0.0000000069417501176d0, 
     &    0.0000000342976459827d0, -0.0000001785317236779d0, 
     &    0.0000009525947257118d0, -0.0000052483007560905d0, 
     &    0.0000302364659535708d0, -0.0001858396115473822d0, 
     &    0.0012634378559425382d0, -0.0102594702201954322d0, 
     &    0.1243625515195050218d0, 1.3888709263595291174d0, 
     &    2.4537365708424422209d0 / 
      data (c(i), i = 26, 38) / 
     &    0.0000000001298977078d0, -0.0000000008029574890d0, 
     &    0.0000000049454846150d0, -0.0000000317563534834d0, 
     &    0.0000002092136698089d0, -0.0000014252023958462d0, 
     &    0.0000101652510114008d0, -0.0000774550502862323d0, 
     &    0.0006537746948291078d0, -0.0066014912535521830d0, 
     &    0.0996711934948138193d0, 1.6110931485817511402d0, 
     &    3.9578139676187162939d0 / 
      data (c(i), i = 39, 51) / 
     &    0.0000000000183995642d0, -0.0000000001353537034d0, 
     &    0.0000000009984676809d0, -0.0000000076346363974d0, 
     &    0.0000000599311464148d0, -0.0000004868554120177d0, 
     &    0.0000041441957716669d0, -0.0000377160856623282d0, 
     &    0.0003805693126824884d0, -0.0045979851178130194d0, 
     &    0.0831422678749791178d0, 1.7929113303999329439d0, 
     &    5.6625620598571415285d0 / 
      data (c(i), i = 52, 64) / 
     &    0.0000000000034858778d0, -0.0000000000297587783d0, 
     &    0.0000000002557677575d0, -0.0000000022705728282d0, 
     &    0.0000000207024992450d0, -0.0000001954426390917d0, 
     &    0.0000019343161886722d0, -0.0000204790249102570d0, 
     &    0.0002405181940241215d0, -0.0033842087561074799d0, 
     &    0.0713079483483518997d0, 1.9467574842460867884d0, 
     &    7.5343642367587329552d0 / 
      data (d(i), i = 0, 6) / 
     &    -0.00163312359200500807d0, 0.00083644533703385956d0, 
     &    -0.00059518947575728181d0, 0.00079365057505415415d0, 
     &    -0.00277777777735463043d0, 0.08333333333333309869d0, 
     &    0.91893853320467274178d0 / 
      w = x
      if (x .lt. 0) w = 1 - x
      if (w .lt. 0.5d0) then
          k = 0
          if (w .ge. 0.25d0) k = 11
          y = ((((((((((a(k) * w + a(k + 1)) * w + 
     &        a(k + 2)) * w + a(k + 3)) * w + a(k + 4)) * w + 
     &        a(k + 5)) * w + a(k + 6)) * w + a(k + 7)) * w + 
     &        a(k + 8)) * w + a(k + 9)) * w + a(k + 10)) * w
          y = -log(y)
      else if (w .lt. 3.5d0) then
          t = w - 4.5d0 / (w + 0.5d0)
          k = int(t + 4)
          t = t - (k - 3.5d0)
          k = k * 14
          y = ((((((((((((b(k) * t + b(k + 1)) * t + 
     &        b(k + 2)) * t + b(k + 3)) * t + b(k + 4)) * t + 
     &        b(k + 5)) * t + b(k + 6)) * t + b(k + 7)) * t + 
     &        b(k + 8)) * t + b(k + 9)) * t + b(k + 10)) * t + 
     &        b(k + 11)) * t + b(k + 12)) * t + b(k + 13)
      else if (w .lt. 8) then
          k = (int(w)) - 3
          t = w - (k + 3.5d0)
          k = k * 13
          y = (((((((((((c(k) * t + c(k + 1)) * t + 
     &        c(k + 2)) * t + c(k + 3)) * t + c(k + 4)) * t + 
     &        c(k + 5)) * t + c(k + 6)) * t + c(k + 7)) * t + 
     &        c(k + 8)) * t + c(k + 9)) * t + c(k + 10)) * t + 
     &        c(k + 11)) * t + c(k + 12)
      else
          v = 1 / w
          t = v * v
          y = (((((d(0) * t + d(1)) * t + d(2)) * t + 
     &        d(3)) * t + d(4)) * t + d(5)) * v + d(6)
          y = y + ((w - 0.5d0) * log(w) - w)
      end if
      if (x .lt. 0) y = log(pi / sin(pi * x)) - y
      dlgamma = y
      end
!