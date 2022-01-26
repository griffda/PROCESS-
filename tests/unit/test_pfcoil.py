"""Unit tests for pfcoil.f90."""
import pytest
from process.pfcoil import PFCoil
from process.fortran import pfcoil_module as pf
import numpy as np
from numpy.testing import assert_array_almost_equal
from typing import NamedTuple


@pytest.fixture
def pfcoil():
    """Fixture to create a PFCoil object.

    :return: an instance of PFCoil
    :rtype: process.pfcoil.PFCoil
    """
    pfcoil = PFCoil()

    return pfcoil


def test_init_pfcoil(pfcoil):
    """Test initialisation of Fortran module variables.

    :param pfcoil: PFCoil object
    :type pfcoil: process.pfcoil.PFCoil
    """
    # Test a selection of module variables
    assert pf.ssq0 == 0.0
    assert pf.cslimit == False
    assert pf.nef == 0


def test_rsid():
    """Test rsid subroutine.

    rsid() requires specific arguments in order to work; these were discovered
    using gdb to break on the first subroutine call when running the baseline
    2018 IN.DAT.
    """
    nptsmx = 32
    npts = 32
    brin = np.zeros(nptsmx)
    bzin = np.zeros(nptsmx)
    nfix = 14
    ngrp = 4
    ccls = np.array(
        [
            14742063.826112622,
            20032681.634901665,
            580406.62653667293,
            429746.74788703024,
            0,
            0,
            0,
            0,
            0,
            0,
        ]
    )
    brssq = 1.7347234759768071e-18
    brnrm = 0

    bzssq = 164.92735114640433
    bznrm = -0.12924373312291032
    ssq = 0
    bfix = np.array(
        [
            -2.7755575615628914e-17,
            -2.7755575615628914e-17,
            -2.0816681711721685e-17,
            -6.2450045135165055e-17,
            4.8572257327350599e-17,
            8.3266726846886741e-17,
            6.9388939039072284e-18,
            4.163336342344337e-17,
            2.0816681711721685e-17,
            1.3877787807814457e-17,
            -2.7755575615628914e-17,
            -2.7755575615628914e-17,
            1.3877787807814457e-17,
            3.1225022567582528e-17,
            -1.3877787807814457e-17,
            -3.4694469519536142e-18,
            6.9388939039072284e-18,
            -2.7755575615628914e-17,
            3.4694469519536142e-18,
            2.4286128663675299e-17,
            4.5102810375396984e-17,
            1.3877787807814457e-17,
            1.0408340855860843e-17,
            -6.9388939039072284e-18,
            1.3877787807814457e-17,
            1.7347234759768071e-17,
            2.7755575615628914e-17,
            -6.9388939039072284e-18,
            -1.7347234759768071e-18,
            2.2551405187698492e-17,
            1.0408340855860843e-17,
            1.7347234759768071e-18,
            -0.3537283013510894,
            -0.34304632621819631,
            -0.33256831505329448,
            -0.32230579414441957,
            -0.31226839635096026,
            -0.30246398750499448,
            -0.29289879096799015,
            -0.283577511993583,
            -0.27450346141940818,
            -0.26567867760304337,
            -0.25710404545385829,
            -0.24877941153731781,
            -0.24070369440957809,
            -0.23287498952924324,
            -0.22529066827105398,
            -0.21794747072660745,
            -0.21084159211740575,
            -0.20396876276560738,
            -0.19732432166849023,
            -0.1909032838051643,
            -0.18470040136999577,
            -0.17871021917825794,
            -0.17292712452755193,
            -0.16734539182494823,
            -0.16195922230675014,
            -0.15676277918619164,
            -0.15175021856634768,
            -0.1469157164516591,
            -0.14225349218352654,
            -0.13775782861391137,
            -0.13342308931695324,
            -0.12924373312291032,
            6.9533558060713876e-310,
            3.7299818735403947e-315,
            1.2461007517394582e-316,
            9.3633631912996395e-97,
            6.9533558060721781e-310,
            6.9533472776350595e-310,
            6.9533558069464767e-310,
            6.9533558071276999e-310,
            6.9533558060745496e-310,
            3.7299818735403947e-315,
        ]
    )
    gmat = np.reshape(
        [
            -7.5172758677351012e-09,
            -7.5647853749229854e-09,
            -7.5997238671870255e-09,
            -7.6223532503471955e-09,
            -7.632981272566719e-09,
            -7.6319588203836032e-09,
            -7.6196767980524902e-09,
            -7.5965626403274021e-09,
            -7.5630765157560611e-09,
            -7.5197072828239948e-09,
            -7.4669682647618112e-09,
            -7.4053929104575667e-09,
            -7.3355304087267682e-09,
            -7.257941321277872e-09,
            -7.1731932962298625e-09,
            -7.0818569191945103e-09,
            -6.984501752975301e-09,
            -6.8816926101232378e-09,
            -6.773986095202415e-09,
            -6.6619274459294016e-09,
            -6.5460476946140002e-09,
            -6.4268611637803582e-09,
            -6.304863302684304e-09,
            -6.1805288648306937e-09,
            -6.0543104206590232e-09,
            -5.9266371943928248e-09,
            -5.7979142096894936e-09,
            -5.6685217251979314e-09,
            -5.5388149384199096e-09,
            -5.4091239343399337e-09,
            -5.2797538540809693e-09,
            -5.1509852582910393e-09,
            9.3492430453057107e-09,
            9.0085911649292264e-09,
            8.6689173990086107e-09,
            8.3309944868826759e-09,
            7.9955690099997426e-09,
            7.6633571228220887e-09,
            7.3350406224857193e-09,
            7.0112634126980615e-09,
            6.6926284084926325e-09,
            6.3796949185645214e-09,
            6.0729765313335842e-09,
            5.7729395199931187e-09,
            5.4800017709631934e-09,
            5.1945322297232159e-09,
            4.9168508482561741e-09,
            4.6472290095579218e-09,
            4.3858903970555656e-09,
            4.1330122704854838e-09,
            3.8887271048890237e-09,
            3.6531245459172901e-09,
            3.4262536325698331e-09,
            3.2081252377476782e-09,
            2.9987146774668041e-09,
            2.7979644411114181e-09,
            2.6057869975431741e-09,
            2.4220676350521897e-09,
            2.2466672968619253e-09,
            2.0794253780146199e-09,
            1.9201624538069016e-09,
            1.7686829143744347e-09,
            1.6247774844145175e-09,
            1.4882256112814138e-09,
            5.0000000000000003e-10,
            0,
            0,
            0,
            0.62007430335325187,
            0.63886443375789592,
            0.65765456416253987,
            0.67644469456718381,
            0.69523482497182787,
            0.71402495537647193,
            5.2231355975329339e-09,
            5.2711960625139632e-09,
            5.311576236174689e-09,
            5.3443982499402794e-09,
            5.3698063638504674e-09,
            5.3879658378724807e-09,
            5.3990616639723659e-09,
            5.4032971732511661e-09,
            5.400892534248167e-09,
            5.3920831599635704e-09,
            5.3771180422303474e-09,
            5.356258032750445e-09,
            5.3297740904008349e-09,
            5.2979455143168409e-09,
            5.2610581817914976e-09,
            5.2194028092188655e-09,
            5.1732732531934983e-09,
            5.1229648674991946e-09,
            5.0687729301276593e-09,
            5.0109911527105061e-09,
            4.9499102828790943e-09,
            4.8858168081356574e-09,
            4.8189917678740469e-09,
            4.7497096782738378e-09,
            4.6782375729467911e-09,
            4.6048341604722139e-09,
            4.5297490983470443e-09,
            4.4532223814171667e-09,
            4.3754838415645443e-09,
            4.2967527543090933e-09,
            4.2172375470486835e-09,
            4.1371356029042306e-09,
            7.3034474444379776e-09,
            7.0812955124293447e-09,
            6.8590392930553784e-09,
            6.637109454243546e-09,
            6.4159243604656496e-09,
            6.1958883508443865e-09,
            5.977390136484671e-09,
            5.7608013330362493e-09,
            5.5464751423657795e-09,
            5.3347451948431544e-09,
            5.1259245611905599e-09,
            4.920304940172507e-09,
            4.7181560256885825e-09,
            4.5197250541359462e-09,
            4.3252365302988979e-09,
            4.1348921275568188e-09,
            3.9488707559297158e-09,
            3.7673287894451147e-09,
            3.5904004425437655e-09,
            3.4181982837673617e-09,
            3.2508138738027688e-09,
            3.0883185140970476e-09,
            2.9307640917016306e-09,
            2.7781840057391115e-09,
            2.6305941608925159e-09,
            2.4879940135710844e-09,
            2.350367656878777e-09,
            2.2176849311714561e-09,
            2.0899025478031295e-09,
            1.9669652145980284e-09,
            1.8488067526120262e-09,
            1.7353511948326436e-09,
            0,
            5.0000000000000003e-10,
            0,
            0,
            2.0105439532969078,
            2.0293340837015519,
            2.0481242141061959,
            2.06691434451084,
            2.0857044749154836,
            2.1044946053201277,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            7.0796316577555638e-08,
            7.1129087582983064e-08,
            7.147530005952651e-08,
            7.1835291378861722e-08,
            7.2209413650970811e-08,
            7.2598033968782154e-08,
            7.3001534603899801e-08,
            7.3420313139375502e-08,
            7.3854782522663518e-08,
            7.4305371018539935e-08,
            7.4772522037762321e-08,
            7.5256693812462047e-08,
            7.5758358883548959e-08,
            7.6278003358579342e-08,
            7.6816125890374903e-08,
            7.7373236316910148e-08,
            7.794985389128976e-08,
            7.8546505016623934e-08,
            7.9163720383809578e-08,
            7.9802031390073597e-08,
            8.04619656919452e-08,
            8.1144041717279125e-08,
            8.1848761926055365e-08,
            8.2576604567746986e-08,
            8.3328013632647066e-08,
            8.4103386633970471e-08,
            8.4903059784765231e-08,
            8.5727290046267352e-08,
            8.6576233419482551e-08,
            8.7449918726163595e-08,
            8.8348215975168323e-08,
            8.9270798231114913e-08,
            0,
            0,
            1.0000000000000001e-09,
            0,
            3.4010136032405636,
            3.4198037336452076,
            3.4385938640498517,
            3.4573839944544953,
            3.4761741248591393,
            3.4949642552637834,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            5.4813906485884661e-08,
            5.4774799801531037e-08,
            5.4730107534112241e-08,
            5.4679342869890839e-08,
            5.4621991000204629e-08,
            5.4557508051195313e-08,
            5.4485320038324824e-08,
            5.4404821860060912e-08,
            5.4315376347382176e-08,
            5.4216313388257004e-08,
            5.4106929148993813e-08,
            5.3986485417343063e-08,
            5.385420909545036e-08,
            5.3709291874199423e-08,
            5.3550890124120389e-08,
            5.3378125041838762e-08,
            5.3190083094948163e-08,
            5.2985816812140533e-08,
            5.2764345969327501e-08,
            5.2524659226222869e-08,
            5.2265716271286847e-08,
            5.1986450535883233e-08,
            5.1685772540768793e-08,
            5.1362573939377029e-08,
            5.1015732322501792e-08,
            5.064411684761966e-08,
            5.0246594752874167e-08,
            4.982203881031119e-08,
            4.9369335764923977e-08,
            4.8887395795052735e-08,
            4.8375163015320454e-08,
            4.7831627025242824e-08,
            0,
            0,
            0,
            1.0000000000000001e-09,
            25.13158605014862,
            25.025324207304788,
            24.917316885291271,
            24.807573484682806,
            24.696103604411817,
            24.582917043712971,
            24.468023804127618,
            24.351434091569946,
            24.233158318456688,
            24.113207105902351,
            23.991591285982111,
            23.868321904064398,
            23.743410221215505,
            23.616867716678527,
            23.488706090429144,
            23.358937265810724,
            23.227573392251532,
            23.094626848066817,
            22.960110243348691,
            22.824036422946961,
            22.686418469544087,
            22.547269706827709,
            22.406603702764219,
            22.264434272977205,
            22.120775484234603,
            21.975641658048701,
            21.829047374393273,
            21.68100747554244,
            21.531537070035913,
            21.380651536775758,
            21.228366529259795,
            21.074697979957286,
            20.919662104832742,
            20.763275408023894,
            20.605554686680403,
            20.446517035970114,
            20.286179854260013,
            20.124560848479536,
            19.961678039674222,
            19.79754976875827,
            19.632194702474806,
            19.46563183957354,
            19.297880517215713,
            19.128960417617037,
            18.958891574939837,
            18.787694382446446,
            18.615389599926406,
            18.441998361411049,
            18.267542183189661,
            18.092042972142579,
            17.915523034407233,
            17.738005084394501,
            17.559512254173683,
            17.380068103245705,
            17.199696628725381,
            17.018422275955185,
            16.836269949574284,
            16.653265025068411,
            16.469433360827917,
            16.284801310743315,
            16.099395737369765,
            15.91324402569419,
            15.726374097541433,
            15.538814426658366,
            15.350594054518114,
            15.161742606889613,
            14.972290311221556,
            14.782268014893495,
            14.591707204391369,
            14.400640025469301,
            14.209099304364994,
            14.017118570141474,
            13.824732078234575,
            13.631974835292443,
            13.438882625401121,
            13.245492037798916,
            13.051840496191812,
            12.857966289792909,
            12.663908606220593,
            12.469707566403567,
            12.275404261655641,
            12.081040793099827,
            11.886660313639975,
            11.69230707269924,
            11.498026463968529,
            11.303865076434823,
            11.109870748989817,
            10.916092628954088,
            10.722581234891317,
            10.529388524132496,
            10.33656796548159,
            10.144174617633903,
            9.9522652139068501,
            9.760898253962452,
            9.5701341032929843,
            9.3800351013488772,
            9.1906656793134758,
            9.0020924886770519,
            8.8143845419364979,
            8.6276133669531312,
            8.4418531767464202,
            8.2571810567943729,
            8.0736771722637322,
            7.8914249980183939,
            7.7105115747715134,
            7.5310277953788134,
            7.3530687260479235,
            7.1767339682022557,
            7.0021280679401254,
            6.8293609815428935,
            6.658548607405363,
            6.4898133972199918,
            6.32328506242591,
            6.1591013960900813,
            5.9974092358858009,
            5.8383656011988148,
            5.6821390473972109,
            5.5289112941053018,
            5.3788792036934145,
            5.2322572139005272,
            5.0892803689803294,
            4.9502081543226453,
            4.8153294326235061,
            4.6849689274151469,
            4.5594959428242898,
            4.4393364257562693,
            4.3249902307904238,
            4.2170568981487575,
            4.1162762690671855,
            4.0235971899548915,
            3.9403058021811721,
            3.8683035615989403,
            3.8108886937263349,
            3.7775374842470044,
            3.4710760272264269,
            3.1646145702058428,
            2.8581531131852587,
            2.5516916561646745,
            2.2452301991440899,
            1.9387687421235056,
            1.6323072851029214,
            1.3258458280823373,
            1.0193843710617527,
            0.71292291404116859,
            0.40646145702058434,
            0.10000000000000001,
            9.7755857974627389,
            9.7752508937483391,
            9.774692720891009,
            9.7739112788907452,
            9.7729065677475511,
            9.7716785874614214,
            9.7702273380323632,
            9.7685528194603712,
            9.7666550317454472,
            9.7645339748875912,
            9.7621896488868014,
            9.7596220537430796,
            9.7568311894564275,
            9.7538170560268416,
            9.7505796534543254,
            9.7471189817388737,
            9.7434350408804917,
            9.7395278308791777,
            9.7353973517349299,
            9.7310436034477519,
            9.72646658601764,
            9.7216662994445961,
            9.7166427437286185,
            9.7113959188697123,
            9.7059258248678706,
            9.7002324617230968,
            9.6943158294353928,
            9.688175928004755,
            9.6818127574311852,
            9.6752263177146851,
            9.6684166088552512,
            9.6613836308528835,
            9.6541273837075856,
            9.6466478674193539,
            9.6389450819881919,
            9.6310190274140961,
            9.6228697036970683,
            9.6144971108371085,
            9.6059012488342166,
            9.5970821176883927,
            9.588039717399635,
            9.5787740479679453,
            9.5692851093933253,
            9.5595729016757716,
            9.5496374248152858,
            9.5394786788118697,
            9.5290966636655181,
            9.5184913793762345,
            9.5076628259440188,
            9.4966110033688729,
            9.4853359116507932,
            9.4738375507897832,
            9.4621159207858376,
            9.4501710216389618,
            9.4380028533491522,
            9.4256114159164124,
            9.4129967093407405,
            9.4001587336221348,
            9.3870974887605971,
            9.3738129747561274,
            9.3603051916087257,
            9.3465741393183919,
            9.3326198178851261,
            9.3184422273089247,
            9.3040413675897948,
            9.2894172387277312,
            9.2745698407227355,
            9.2594991735748078,
            9.2442052372839481,
            9.2286880318501545,
            9.2129475572734307,
            9.1969838135537749,
            9.1807968006911853,
            9.1643865186856637,
            9.14775296753721,
            9.1308961472458225,
            9.1138160578115048,
            9.0965126992342551,
            9.0789860715140716,
            9.061236174650956,
            9.0432630086449102,
            9.0250665734959306,
            9.0066468692040189,
            8.9880038957691752,
            8.9691376531913978,
            8.9500481414706883,
            8.9307353606070485,
            8.911199310600475,
            8.8914399914509694,
            8.8714574031585318,
            8.8512515457231622,
            8.8308224191448588,
            8.8101700234236251,
            8.7892943585594594,
            8.7681954245523599,
            8.7468732214023284,
            8.725327749109363,
            8.7035590076734675,
            8.6815669970946399,
            8.6593517173728802,
            8.6369131685081868,
            8.6142513505005631,
            8.5913662633500039,
            8.5682579070565161,
            8.5449262816200946,
            8.5213713870407393,
            8.4975932233184537,
            8.4735917904532361,
            8.4493670884450864,
            8.4249191172940012,
            8.4002478769999875,
            8.3753533675630401,
            8.3502355889831605,
            8.3248945412603472,
            8.2993302243946037,
            8.2735426383859281,
            8.2475317832343187,
            8.2212976589397773,
            8.1948402655023038,
            8.1681596029218984,
            8.1412556711985609,
            8.1141284703322896,
            8.086778000323088,
            8.0592042611709527,
            8.0314072528758871,
            8.0033869754378877,
            7.9751434288569545,
            7.946676613133091,
            7.9179865282662956,
            7.8890731742565681,
            7.8599365511039059,
            7.8305766588083143,
            7.8009934973697881,
            7.7711870667883316,
            7.7411573670639413,
            7.7109043981966199,
            7.6804281601863664,
            7.6497286530331801,
            7.6188058767370617,
            7.5876598312980104,
            7.5562905167160279,
            7.5246979329911108,
            7.4928820801232634,
            7.460842958112484,
            7.4285805669587708,
            7.3960949066621273,
            7.3633859772225492,
            7.3304537786400417,
            7.2972983109146003,
            7.2639195740462261,
            7.2303175680349208,
            7.1964922928806816,
            7.1624437485835113,
            7.1281719351434081,
            7.0936768525603737,
            7.0589585008344073,
            7.0240168799655063,
            6.9888519899536758,
            6.9534638307989116,
            6.9178524025012154,
            6.8820177050605871,
            6.8459597384770268,
            6.8096785027505335,
            6.7731739978811083,
            6.7364462238687519,
            6.6994951807134608,
            6.6623208684152386,
            6.6249232869740844,
            6.5873024363899981,
            6.549458316662979,
            6.5113909277930277,
            6.4731002697801445,
            6.4345863426243284,
            6.3958491463255811,
            6.3568886808839,
            6.3177049462992887,
            6.2782979425717436,
            6.2386676697012664,
            6.1988141276878572,
            6.158737316531516,
            6.1184372362322419,
            6.0779138867900357,
            6.0371672682048976,
            5.9961973804768274,
            5.9550042236058234,
            5.9135877975918882,
            5.8719481024350211,
            5.8300851381352228,
            5.630033197120782,
            5.4299812561063376,
            5.2299293150918933,
            5.0298773740774489,
            4.8298254330630046,
            4.6297734920485603,
            4.4297215510341159,
            4.2296696100196716,
            4.0296176690052263,
            3.8295657279907829,
            3.6295137869763385,
            3.4294618459618942,
            0.99993750390600578,
            0.99975006248437881,
            0.99943781622837147,
            0.99900099900099915,
            0.99843993759750393,
            0.99775505113494634,
            0.9969468502710449,
            0.99601593625498008,
            0.99496299981344449,
            0.99378881987577627,
            0.9924942621425471,
            0.99108027750247785,
            0.98954790030304907,
            0.98789824648061242,
            0.98613251155624027,
            0.98425196850393704,
            0.98225796549818889,
            0.98015192354814984,
            0.97793533402603749,
            0.97560975609756106,
            0.97317681406240497,
            0.97063819461295786,
            0.96799564401960192,
            0.96525096525096521,
            0.96240601503759393,
            0.95946270088750307,
            0.95642297806204801,
            0.95328884652049573,
            0.95006234784157706,
            0.94674556213017758,
            0.94334060491716298,
            0.93984962406015038,
            0.93627479665281765,
            0.93261832595010496,
            0.92888243831640049,
            0.92506938020351526,
            0.92118141516494911,
            0.91722082091263479,
            0.91318988642200782,
            0.90909090909090906,
            0.90492619195746837,
            0.90069804098176087,
            0.89640876239565237,
            0.89206066012488849,
            0.8876560332871013,
            0.88319717376904394,
            0.87868636388599042,
            0.87412587412587417,
            0.86951796098038148,
            0.86486486486486491,
            0.86016880812859531,
            0.85543199315654406,
            0.85065660056355996,
            0.84584478748149716,
            0.84099868593955329,
            0.83612040133779264,
            0.83121201101355924,
            0.82627556290022719,
            0.82131307427750111,
            0.81632653061224481,
            0.81131788448861619,
            0.80628905462608336,
            0.80124192498372471,
            0.79617834394904463,
            0.7911001236093943,
            0.78600903910394959,
            0.78090682805407774,
            0.77579519006982145,
            0.77067578633013822,
            0.76555023923444976,
            0.76042013212299797,
            0.75528700906344415,
            0.75015237470111118,
            0.74501769417023656,
            0.73988439306358378,
            0.73475385745775168,
            0.72962743399151797,
            0.72450642999456616,
            0.71939211366395395,
            0.71428571428571419,
            0.70918842249900271,
            0.70410139060024657,
            0.69902573288479186,
            0.69396252602359476,
            0.68891280947255107,
            0.68387758591212167,
            0.67885782171496456,
            0.67385444743935308,
            0.66886835834622305,
            0.66390041493775931,
            0.65895144351550594,
            0.65402223675604976,
            0.6491135543024058,
            0.64422612336930263,
            0.63936063936063936,
            0.63451776649746194,
            0.62969813845487821,
            0.62490235900640523,
            0.62013100267431487,
            0.61538461538461542,
            0.61066371512537687,
            0.60596879260718073,
            0.60130031192453681,
            0.59665871121718372,
        ],
        (74, 10),
        order="F",
    )

    brssq_exp = 0.0005682062650683896
    brnrm_exp = 0.0
    bzssq_exp = 7.188474446020582e-05
    bznrm_exp = 0.0
    ssq_exp = 0.0006400910095285954

    brssq, brnrm, bzssq, bznrm, ssq = pf.rsid(
        npts, brin, bzin, nfix, ngrp, ccls, bfix, gmat
    )

    assert brssq == pytest.approx(brssq_exp)
    assert brnrm == pytest.approx(brnrm_exp)
    assert bzssq == pytest.approx(bzssq_exp)
    assert bznrm == pytest.approx(bznrm_exp)
    assert ssq == pytest.approx(ssq_exp)


def test_bfield():
    """Test bfield subroutine.

    bfield() requires specific arguments in order to work; these were discovered
    using gdb to break on the first subroutine call when running the baseline
    2018 IN.DAT.
    """
    rc = np.array(
        [
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
            2.6084100000000001,
        ]
    )
    zc = np.array(
        [
            0.58327007281470211,
            1.7498102184441064,
            2.9163503640735104,
            4.0828905097029144,
            5.2494306553323193,
            6.4159708009617233,
            7.5825109465911273,
            -0.58327007281470211,
            -1.7498102184441064,
            -2.9163503640735104,
            -4.0828905097029144,
            -5.2494306553323193,
            -6.4159708009617233,
            -7.5825109465911273,
        ]
    )
    cc = np.array(
        [
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
            12444820.564847374,
        ]
    )
    rp = 6.0223258064516134
    zp = 0

    xc_exp = np.array(
        [
            2.36278088e-06,
            2.05233185e-06,
            1.62139434e-06,
            1.22298265e-06,
            9.08339602e-07,
            6.75290638e-07,
            5.06601647e-07,
            2.36278088e-06,
            2.05233185e-06,
            1.62139434e-06,
            1.22298265e-06,
            9.08339602e-07,
            6.75290638e-07,
            5.06601647e-07,
        ]
    )
    br_exp = 0.0
    bz_exp = 0.0
    psi_exp = 0.0

    xc, br, bz, psi = pf.bfield(rc, zc, cc, rp, zp)

    assert_array_almost_equal(xc, xc_exp)
    # TODO Correct incorrect pytest.approx() usage
    assert pytest.approx(br, br_exp)
    assert pytest.approx(bz, bz_exp)
    assert pytest.approx(psi, psi_exp)


class BfmaxTestAsset(NamedTuple):
    """Test asset for single test case of test_bfmax().

    :param NamedTuple: Typed version of namedtuple
    :type NamedTuple: typing.NamedTuple

    TODO What's the best way to doc a NamedTuple?
    :a: solenoid inner radius (m)
    :type a: float
    :h: solenoid half height (m)
    :type h: float
    :bfmax_exp: expected returned value of bfmax, the maximum field of a
    solenoid
    :type bfmax_exp: float
    """
    a: float
    h: float
    bfmax_exp: float


@pytest.mark.parametrize(
    "test_asset",
    [
        BfmaxTestAsset(a=2.0, h=8.0, bfmax_exp=2.461485e1),
        BfmaxTestAsset(a=2.0, h=4.1, bfmax_exp=2.2072637e1),
        BfmaxTestAsset(a=2.0, h=2.1, bfmax_exp=1.803889e1),
        BfmaxTestAsset(a=2.0, h=1.6, bfmax_exp=1.693509e1),
        BfmaxTestAsset(a=2.0, h=1.0, bfmax_exp=1.4601048e1),
    ],
)
def test_bfmax(test_asset):
    """Test bfmax() function.

    :param test_asset: arguments and expected return value for single test case
    :type test_asset: BfmaxTestAsset
    """
    rj = 2.0e7
    b = 3.0

    bfmax = pf.bfmax(rj, test_asset.a, b, test_asset.h)

    assert pytest.approx(bfmax) == test_asset.bfmax_exp
