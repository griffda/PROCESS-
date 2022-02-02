import pytest
from typing import NamedTuple, Any
from process.fortran import current_drive_variables
from process.fortran import fwbs_variables
from process.fortran import buildings_variables
from process.fortran import physics_variables
from process.fortran import cost_variables
from process.fortran import pfcoil_variables
from process.fortran import tfcoil_variables
from process.fortran import build_variables
from process.fortran import divertor_variables
from process.fortran import buildings_module


class BldgsSizesParam(NamedTuple):
    i_bldgs_v: Any
    efloor: Any
    volnucb: Any
    bioshld_thk: Any
    reactor_wall_thk: Any
    reactor_roof_thk: Any
    reactor_fndtn_thk: Any
    reactor_clrnc: Any
    transp_clrnc: Any
    cryostat_clrnc: Any
    ground_clrnc: Any
    crane_clrnc_h: Any
    crane_arm_h: Any
    reactor_hall_l: Any
    reactor_hall_w: Any
    reactor_hall_h: Any
    nbi_sys_l: Any
    nbi_sys_w: Any
    fc_building_l: Any
    fc_building_w: Any
    warm_shop_l: Any
    warm_shop_w: Any
    warm_shop_h: Any
    workshop_l: Any
    workshop_w: Any
    workshop_h: Any
    robotics_l: Any
    robotics_w: Any
    robotics_h: Any
    maint_cont_l: Any
    maint_cont_w: Any
    maint_cont_h: Any
    turbine_hall_l: Any
    turbine_hall_w: Any
    turbine_hall_h: Any
    gas_buildings_l: Any
    gas_buildings_w: Any
    gas_buildings_h: Any
    water_buildings_l: Any
    water_buildings_w: Any
    water_buildings_h: Any
    sec_buildings_l: Any
    sec_buildings_w: Any
    sec_buildings_h: Any
    staff_buildings_area: Any
    staff_buildings_h: Any
    hcd_building_l: Any
    hcd_building_w: Any
    hcd_building_h: Any
    magnet_pulse_l: Any
    magnet_pulse_w: Any
    magnet_pulse_h: Any
    magnet_trains_l: Any
    magnet_trains_w: Any
    magnet_trains_h: Any
    control_buildings_l: Any
    control_buildings_w: Any
    control_buildings_h: Any
    ilw_smelter_l: Any
    ilw_smelter_w: Any
    ilw_smelter_h: Any
    ilw_storage_l: Any
    ilw_storage_w: Any
    ilw_storage_h: Any
    llw_storage_l: Any
    llw_storage_w: Any
    llw_storage_h: Any
    hw_storage_l: Any
    hw_storage_w: Any
    hw_storage_h: Any
    tw_storage_l: Any
    tw_storage_w: Any
    tw_storage_h: Any
    auxcool_l: Any
    auxcool_w: Any
    auxcool_h: Any
    cryomag_l: Any
    cryomag_w: Any
    cryomag_h: Any
    cryostore_l: Any
    cryostore_w: Any
    cryostore_h: Any
    elecdist_l: Any
    elecdist_w: Any
    elecdist_h: Any
    elecstore_l: Any
    elecstore_w: Any
    elecstore_h: Any
    elecload_l: Any
    elecload_w: Any
    elecload_h: Any
    chemlab_l: Any
    chemlab_w: Any
    chemlab_h: Any
    heat_sink_l: Any
    heat_sink_w: Any
    heat_sink_h: Any
    aux_build_l: Any
    aux_build_w: Any
    aux_build_h: Any
    qnty_sfty_fac: Any
    hotcell_h: Any
    hot_sepdist: Any
    iefrf: Any
    n_tf: Any
    i_tf_sup: Any
    pfrmax: Any
    tlife: Any
    cplife: Any
    divlife: Any
    rdewex: Any
    bktlife: Any
    hmax: Any
    tfcth: Any
    tftsgap: Any
    thshield: Any
    shldith: Any
    shldoth: Any
    scrapli: Any
    scraplo: Any
    fwith: Any
    fwoth: Any
    blnkith: Any
    blnkoth: Any
    r_cp_top: Any
    divfix: Any
    rmajor: Any
    rminor: Any
    tf_radial_dim: Any
    tf_vertical_dim: Any
    outfile: Any
    iprint: Any
    expected_reactor_hall_l: Any
    expected_reactor_hall_w: Any
    expected_reactor_hall_h: Any


@pytest.mark.parametrize(
    "bldgssizesparam",
    (
        BldgsSizesParam(
            i_bldgs_v=0,
            efloor=0,
            volnucb=0,
            bioshld_thk=2.5,
            reactor_wall_thk=2,
            reactor_roof_thk=1,
            reactor_fndtn_thk=2,
            reactor_clrnc=4,
            transp_clrnc=1,
            cryostat_clrnc=2.5,
            ground_clrnc=5,
            crane_clrnc_h=4,
            crane_arm_h=10,
            reactor_hall_l=0,
            reactor_hall_w=0,
            reactor_hall_h=0,
            nbi_sys_l=225,
            nbi_sys_w=185,
            fc_building_l=60,
            fc_building_w=60,
            warm_shop_l=100,
            warm_shop_w=50,
            warm_shop_h=10,
            workshop_l=150,
            workshop_w=125,
            workshop_h=10,
            robotics_l=50,
            robotics_w=30,
            robotics_h=30,
            maint_cont_l=125,
            maint_cont_w=100,
            maint_cont_h=6,
            turbine_hall_l=109,
            turbine_hall_w=62,
            turbine_hall_h=15,
            gas_buildings_l=25,
            gas_buildings_w=15,
            gas_buildings_h=5,
            water_buildings_l=110,
            water_buildings_w=10,
            water_buildings_h=5,
            sec_buildings_l=30,
            sec_buildings_w=25,
            sec_buildings_h=6,
            staff_buildings_area=480000,
            staff_buildings_h=5,
            hcd_building_l=70,
            hcd_building_w=40,
            hcd_building_h=25,
            magnet_pulse_l=105,
            magnet_pulse_w=40,
            magnet_pulse_h=5,
            magnet_trains_l=120,
            magnet_trains_w=90,
            magnet_trains_h=5,
            control_buildings_l=80,
            control_buildings_w=60,
            control_buildings_h=6,
            ilw_smelter_l=50,
            ilw_smelter_w=30,
            ilw_smelter_h=30,
            ilw_storage_l=120,
            ilw_storage_w=100,
            ilw_storage_h=8,
            llw_storage_l=45,
            llw_storage_w=20,
            llw_storage_h=5,
            hw_storage_l=20,
            hw_storage_w=10,
            hw_storage_h=5,
            tw_storage_l=90,
            tw_storage_w=30,
            tw_storage_h=5,
            auxcool_l=20,
            auxcool_w=20,
            auxcool_h=5,
            cryomag_l=120,
            cryomag_w=90,
            cryomag_h=5,
            cryostore_l=160,
            cryostore_w=30,
            cryostore_h=20,
            elecdist_l=380,
            elecdist_w=350,
            elecdist_h=5,
            elecstore_l=100,
            elecstore_w=60,
            elecstore_h=12,
            elecload_l=100,
            elecload_w=90,
            elecload_h=3,
            chemlab_l=50,
            chemlab_w=30,
            chemlab_h=6,
            heat_sink_l=160,
            heat_sink_w=80,
            heat_sink_h=12,
            aux_build_l=60,
            aux_build_w=30,
            aux_build_h=5,
            qnty_sfty_fac=2,
            hotcell_h=12,
            hot_sepdist=2,
            iefrf=10,
            n_tf=16,
            i_tf_sup=1,
            pfrmax=18.98258241468535,
            tlife=40,
            cplife=0,
            divlife=0,
            rdewex=19.48258241468535,
            bktlife=0,
            hmax=9.0730900215620327,
            tfcth=1.208,
            tftsgap=0.05000000000000001,
            thshield=0.050000000000000003,
            shldith=0.30000000000000004,
            shldoth=0.80000000000000004,
            scrapli=0.22500000000000003,
            scraplo=0.22500000000000003,
            fwith=0.018000000000000002,
            fwoth=0.018000000000000002,
            blnkith=0.75500000000000012,
            blnkoth=0.98199999999999998,
            r_cp_top=4.20194118510911,
            divfix=0.62100000000000011,
            rmajor=8.8901000000000003,
            rminor=2.8677741935483869,
            tf_radial_dim=14.129464674334221,
            tf_vertical_dim=20.562180043124066,
            outfile=11,
            iprint=0,
            expected_reactor_hall_l=218.89549448811209,
            expected_reactor_hall_w=218.89549448811209,
            expected_reactor_hall_h=67.624360086248132,
        ),
        BldgsSizesParam(
            i_bldgs_v=0,
            efloor=1539392.0963074313,
            volnucb=5212998.1139194397,
            bioshld_thk=2.5,
            reactor_wall_thk=2,
            reactor_roof_thk=1,
            reactor_fndtn_thk=2,
            reactor_clrnc=4,
            transp_clrnc=1,
            cryostat_clrnc=2.5,
            ground_clrnc=5,
            crane_clrnc_h=4,
            crane_arm_h=10,
            reactor_hall_l=218.89549448811209,
            reactor_hall_w=218.89549448811209,
            reactor_hall_h=67.624360086248132,
            nbi_sys_l=225,
            nbi_sys_w=185,
            fc_building_l=60,
            fc_building_w=60,
            warm_shop_l=100,
            warm_shop_w=50,
            warm_shop_h=10,
            workshop_l=150,
            workshop_w=125,
            workshop_h=10,
            robotics_l=50,
            robotics_w=30,
            robotics_h=30,
            maint_cont_l=125,
            maint_cont_w=100,
            maint_cont_h=6,
            turbine_hall_l=109,
            turbine_hall_w=62,
            turbine_hall_h=15,
            gas_buildings_l=25,
            gas_buildings_w=15,
            gas_buildings_h=5,
            water_buildings_l=110,
            water_buildings_w=10,
            water_buildings_h=5,
            sec_buildings_l=30,
            sec_buildings_w=25,
            sec_buildings_h=6,
            staff_buildings_area=480000,
            staff_buildings_h=5,
            hcd_building_l=70,
            hcd_building_w=40,
            hcd_building_h=25,
            magnet_pulse_l=105,
            magnet_pulse_w=40,
            magnet_pulse_h=5,
            magnet_trains_l=120,
            magnet_trains_w=90,
            magnet_trains_h=5,
            control_buildings_l=80,
            control_buildings_w=60,
            control_buildings_h=6,
            ilw_smelter_l=50,
            ilw_smelter_w=30,
            ilw_smelter_h=30,
            ilw_storage_l=120,
            ilw_storage_w=100,
            ilw_storage_h=8,
            llw_storage_l=45,
            llw_storage_w=20,
            llw_storage_h=5,
            hw_storage_l=20,
            hw_storage_w=10,
            hw_storage_h=5,
            tw_storage_l=90,
            tw_storage_w=30,
            tw_storage_h=5,
            auxcool_l=20,
            auxcool_w=20,
            auxcool_h=5,
            cryomag_l=120,
            cryomag_w=90,
            cryomag_h=5,
            cryostore_l=160,
            cryostore_w=30,
            cryostore_h=20,
            elecdist_l=380,
            elecdist_w=350,
            elecdist_h=5,
            elecstore_l=100,
            elecstore_w=60,
            elecstore_h=12,
            elecload_l=100,
            elecload_w=90,
            elecload_h=3,
            chemlab_l=50,
            chemlab_w=30,
            chemlab_h=6,
            heat_sink_l=160,
            heat_sink_w=80,
            heat_sink_h=12,
            aux_build_l=60,
            aux_build_w=30,
            aux_build_h=5,
            qnty_sfty_fac=2,
            hotcell_h=12,
            hot_sepdist=2,
            iefrf=10,
            n_tf=16,
            i_tf_sup=1,
            pfrmax=18.982980877139834,
            tlife=40,
            cplife=0,
            divlife=6.1337250397740126,
            rdewex=19.482980877139834,
            bktlife=19.216116010620578,
            hmax=9.0730900215620327,
            tfcth=1.208,
            tftsgap=0.05000000000000001,
            thshield=0.050000000000000003,
            shldith=0.30000000000000004,
            shldoth=0.80000000000000004,
            scrapli=0.22500000000000003,
            scraplo=0.22500000000000003,
            fwith=0.018000000000000002,
            fwoth=0.018000000000000002,
            blnkith=0.75500000000000012,
            blnkoth=0.98199999999999998,
            r_cp_top=4.20194118510911,
            divfix=0.62100000000000011,
            rmajor=8.8901000000000003,
            rminor=2.8677741935483869,
            tf_radial_dim=14.129464674334221,
            tf_vertical_dim=20.562180043124066,
            outfile=11,
            iprint=0,
            expected_reactor_hall_l=218.897885262839,
            expected_reactor_hall_w=218.897885262839,
            expected_reactor_hall_h=67.624360086248132,
        ),
    ),
)
def test_bldgs_sizes(bldgssizesparam, monkeypatch):
    monkeypatch.setattr(buildings_variables, "i_bldgs_v", bldgssizesparam.i_bldgs_v)
    monkeypatch.setattr(buildings_variables, "efloor", bldgssizesparam.efloor)
    monkeypatch.setattr(buildings_variables, "volnucb", bldgssizesparam.volnucb)
    monkeypatch.setattr(buildings_variables, "bioshld_thk", bldgssizesparam.bioshld_thk)
    monkeypatch.setattr(
        buildings_variables, "reactor_wall_thk", bldgssizesparam.reactor_wall_thk
    )
    monkeypatch.setattr(
        buildings_variables, "reactor_roof_thk", bldgssizesparam.reactor_roof_thk
    )
    monkeypatch.setattr(
        buildings_variables, "reactor_fndtn_thk", bldgssizesparam.reactor_fndtn_thk
    )
    monkeypatch.setattr(
        buildings_variables, "reactor_clrnc", bldgssizesparam.reactor_clrnc
    )
    monkeypatch.setattr(
        buildings_variables, "transp_clrnc", bldgssizesparam.transp_clrnc
    )
    monkeypatch.setattr(
        buildings_variables, "cryostat_clrnc", bldgssizesparam.cryostat_clrnc
    )
    monkeypatch.setattr(
        buildings_variables, "ground_clrnc", bldgssizesparam.ground_clrnc
    )
    monkeypatch.setattr(
        buildings_variables, "crane_clrnc_h", bldgssizesparam.crane_clrnc_h
    )
    monkeypatch.setattr(buildings_variables, "crane_arm_h", bldgssizesparam.crane_arm_h)
    monkeypatch.setattr(
        buildings_variables, "reactor_hall_l", bldgssizesparam.reactor_hall_l
    )
    monkeypatch.setattr(
        buildings_variables, "reactor_hall_w", bldgssizesparam.reactor_hall_w
    )
    monkeypatch.setattr(
        buildings_variables, "reactor_hall_h", bldgssizesparam.reactor_hall_h
    )
    monkeypatch.setattr(buildings_variables, "nbi_sys_l", bldgssizesparam.nbi_sys_l)
    monkeypatch.setattr(buildings_variables, "nbi_sys_w", bldgssizesparam.nbi_sys_w)
    monkeypatch.setattr(
        buildings_variables, "fc_building_l", bldgssizesparam.fc_building_l
    )
    monkeypatch.setattr(
        buildings_variables, "fc_building_w", bldgssizesparam.fc_building_w
    )
    monkeypatch.setattr(buildings_variables, "warm_shop_l", bldgssizesparam.warm_shop_l)
    monkeypatch.setattr(buildings_variables, "warm_shop_w", bldgssizesparam.warm_shop_w)
    monkeypatch.setattr(buildings_variables, "warm_shop_h", bldgssizesparam.warm_shop_h)
    monkeypatch.setattr(buildings_variables, "workshop_l", bldgssizesparam.workshop_l)
    monkeypatch.setattr(buildings_variables, "workshop_w", bldgssizesparam.workshop_w)
    monkeypatch.setattr(buildings_variables, "workshop_h", bldgssizesparam.workshop_h)
    monkeypatch.setattr(buildings_variables, "robotics_l", bldgssizesparam.robotics_l)
    monkeypatch.setattr(buildings_variables, "robotics_w", bldgssizesparam.robotics_w)
    monkeypatch.setattr(buildings_variables, "robotics_h", bldgssizesparam.robotics_h)
    monkeypatch.setattr(
        buildings_variables, "maint_cont_l", bldgssizesparam.maint_cont_l
    )
    monkeypatch.setattr(
        buildings_variables, "maint_cont_w", bldgssizesparam.maint_cont_w
    )
    monkeypatch.setattr(
        buildings_variables, "maint_cont_h", bldgssizesparam.maint_cont_h
    )
    monkeypatch.setattr(
        buildings_variables, "turbine_hall_l", bldgssizesparam.turbine_hall_l
    )
    monkeypatch.setattr(
        buildings_variables, "turbine_hall_w", bldgssizesparam.turbine_hall_w
    )
    monkeypatch.setattr(
        buildings_variables, "turbine_hall_h", bldgssizesparam.turbine_hall_h
    )
    monkeypatch.setattr(
        buildings_variables, "gas_buildings_l", bldgssizesparam.gas_buildings_l
    )
    monkeypatch.setattr(
        buildings_variables, "gas_buildings_w", bldgssizesparam.gas_buildings_w
    )
    monkeypatch.setattr(
        buildings_variables, "gas_buildings_h", bldgssizesparam.gas_buildings_h
    )
    monkeypatch.setattr(
        buildings_variables, "water_buildings_l", bldgssizesparam.water_buildings_l
    )
    monkeypatch.setattr(
        buildings_variables, "water_buildings_w", bldgssizesparam.water_buildings_w
    )
    monkeypatch.setattr(
        buildings_variables, "water_buildings_h", bldgssizesparam.water_buildings_h
    )
    monkeypatch.setattr(
        buildings_variables, "sec_buildings_l", bldgssizesparam.sec_buildings_l
    )
    monkeypatch.setattr(
        buildings_variables, "sec_buildings_w", bldgssizesparam.sec_buildings_w
    )
    monkeypatch.setattr(
        buildings_variables, "sec_buildings_h", bldgssizesparam.sec_buildings_h
    )
    monkeypatch.setattr(
        buildings_variables,
        "staff_buildings_area",
        bldgssizesparam.staff_buildings_area,
    )
    monkeypatch.setattr(
        buildings_variables, "staff_buildings_h", bldgssizesparam.staff_buildings_h
    )
    monkeypatch.setattr(
        buildings_variables, "hcd_building_l", bldgssizesparam.hcd_building_l
    )
    monkeypatch.setattr(
        buildings_variables, "hcd_building_w", bldgssizesparam.hcd_building_w
    )
    monkeypatch.setattr(
        buildings_variables, "hcd_building_h", bldgssizesparam.hcd_building_h
    )
    monkeypatch.setattr(
        buildings_variables, "magnet_pulse_l", bldgssizesparam.magnet_pulse_l
    )
    monkeypatch.setattr(
        buildings_variables, "magnet_pulse_w", bldgssizesparam.magnet_pulse_w
    )
    monkeypatch.setattr(
        buildings_variables, "magnet_pulse_h", bldgssizesparam.magnet_pulse_h
    )
    monkeypatch.setattr(
        buildings_variables, "magnet_trains_l", bldgssizesparam.magnet_trains_l
    )
    monkeypatch.setattr(
        buildings_variables, "magnet_trains_w", bldgssizesparam.magnet_trains_w
    )
    monkeypatch.setattr(
        buildings_variables, "magnet_trains_h", bldgssizesparam.magnet_trains_h
    )
    monkeypatch.setattr(
        buildings_variables, "control_buildings_l", bldgssizesparam.control_buildings_l
    )
    monkeypatch.setattr(
        buildings_variables, "control_buildings_w", bldgssizesparam.control_buildings_w
    )
    monkeypatch.setattr(
        buildings_variables, "control_buildings_h", bldgssizesparam.control_buildings_h
    )
    monkeypatch.setattr(
        buildings_variables, "ilw_smelter_l", bldgssizesparam.ilw_smelter_l
    )
    monkeypatch.setattr(
        buildings_variables, "ilw_smelter_w", bldgssizesparam.ilw_smelter_w
    )
    monkeypatch.setattr(
        buildings_variables, "ilw_smelter_h", bldgssizesparam.ilw_smelter_h
    )
    monkeypatch.setattr(
        buildings_variables, "ilw_storage_l", bldgssizesparam.ilw_storage_l
    )
    monkeypatch.setattr(
        buildings_variables, "ilw_storage_w", bldgssizesparam.ilw_storage_w
    )
    monkeypatch.setattr(
        buildings_variables, "ilw_storage_h", bldgssizesparam.ilw_storage_h
    )
    monkeypatch.setattr(
        buildings_variables, "llw_storage_l", bldgssizesparam.llw_storage_l
    )
    monkeypatch.setattr(
        buildings_variables, "llw_storage_w", bldgssizesparam.llw_storage_w
    )
    monkeypatch.setattr(
        buildings_variables, "llw_storage_h", bldgssizesparam.llw_storage_h
    )
    monkeypatch.setattr(
        buildings_variables, "hw_storage_l", bldgssizesparam.hw_storage_l
    )
    monkeypatch.setattr(
        buildings_variables, "hw_storage_w", bldgssizesparam.hw_storage_w
    )
    monkeypatch.setattr(
        buildings_variables, "hw_storage_h", bldgssizesparam.hw_storage_h
    )
    monkeypatch.setattr(
        buildings_variables, "tw_storage_l", bldgssizesparam.tw_storage_l
    )
    monkeypatch.setattr(
        buildings_variables, "tw_storage_w", bldgssizesparam.tw_storage_w
    )
    monkeypatch.setattr(
        buildings_variables, "tw_storage_h", bldgssizesparam.tw_storage_h
    )
    monkeypatch.setattr(buildings_variables, "auxcool_l", bldgssizesparam.auxcool_l)
    monkeypatch.setattr(buildings_variables, "auxcool_w", bldgssizesparam.auxcool_w)
    monkeypatch.setattr(buildings_variables, "auxcool_h", bldgssizesparam.auxcool_h)
    monkeypatch.setattr(buildings_variables, "cryomag_l", bldgssizesparam.cryomag_l)
    monkeypatch.setattr(buildings_variables, "cryomag_w", bldgssizesparam.cryomag_w)
    monkeypatch.setattr(buildings_variables, "cryomag_h", bldgssizesparam.cryomag_h)
    monkeypatch.setattr(buildings_variables, "cryostore_l", bldgssizesparam.cryostore_l)
    monkeypatch.setattr(buildings_variables, "cryostore_w", bldgssizesparam.cryostore_w)
    monkeypatch.setattr(buildings_variables, "cryostore_h", bldgssizesparam.cryostore_h)
    monkeypatch.setattr(buildings_variables, "elecdist_l", bldgssizesparam.elecdist_l)
    monkeypatch.setattr(buildings_variables, "elecdist_w", bldgssizesparam.elecdist_w)
    monkeypatch.setattr(buildings_variables, "elecdist_h", bldgssizesparam.elecdist_h)
    monkeypatch.setattr(buildings_variables, "elecstore_l", bldgssizesparam.elecstore_l)
    monkeypatch.setattr(buildings_variables, "elecstore_w", bldgssizesparam.elecstore_w)
    monkeypatch.setattr(buildings_variables, "elecstore_h", bldgssizesparam.elecstore_h)
    monkeypatch.setattr(buildings_variables, "elecload_l", bldgssizesparam.elecload_l)
    monkeypatch.setattr(buildings_variables, "elecload_w", bldgssizesparam.elecload_w)
    monkeypatch.setattr(buildings_variables, "elecload_h", bldgssizesparam.elecload_h)
    monkeypatch.setattr(buildings_variables, "chemlab_l", bldgssizesparam.chemlab_l)
    monkeypatch.setattr(buildings_variables, "chemlab_w", bldgssizesparam.chemlab_w)
    monkeypatch.setattr(buildings_variables, "chemlab_h", bldgssizesparam.chemlab_h)
    monkeypatch.setattr(buildings_variables, "heat_sink_l", bldgssizesparam.heat_sink_l)
    monkeypatch.setattr(buildings_variables, "heat_sink_w", bldgssizesparam.heat_sink_w)
    monkeypatch.setattr(buildings_variables, "heat_sink_h", bldgssizesparam.heat_sink_h)
    monkeypatch.setattr(buildings_variables, "aux_build_l", bldgssizesparam.aux_build_l)
    monkeypatch.setattr(buildings_variables, "aux_build_w", bldgssizesparam.aux_build_w)
    monkeypatch.setattr(buildings_variables, "aux_build_h", bldgssizesparam.aux_build_h)
    monkeypatch.setattr(
        buildings_variables, "qnty_sfty_fac", bldgssizesparam.qnty_sfty_fac
    )
    monkeypatch.setattr(buildings_variables, "hotcell_h", bldgssizesparam.hotcell_h)
    monkeypatch.setattr(buildings_variables, "hot_sepdist", bldgssizesparam.hot_sepdist)
    monkeypatch.setattr(current_drive_variables, "iefrf", bldgssizesparam.iefrf)
    monkeypatch.setattr(tfcoil_variables, "n_tf", bldgssizesparam.n_tf)
    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", bldgssizesparam.i_tf_sup)
    monkeypatch.setattr(pfcoil_variables, "pfrmax", bldgssizesparam.pfrmax)
    monkeypatch.setattr(cost_variables, "tlife", bldgssizesparam.tlife)
    monkeypatch.setattr(cost_variables, "cplife", bldgssizesparam.cplife)
    monkeypatch.setattr(cost_variables, "divlife", bldgssizesparam.divlife)
    monkeypatch.setattr(fwbs_variables, "rdewex", bldgssizesparam.rdewex)
    monkeypatch.setattr(fwbs_variables, "bktlife", bldgssizesparam.bktlife)
    monkeypatch.setattr(build_variables, "hmax", bldgssizesparam.hmax)
    monkeypatch.setattr(build_variables, "tfcth", bldgssizesparam.tfcth)
    monkeypatch.setattr(build_variables, "tftsgap", bldgssizesparam.tftsgap)
    monkeypatch.setattr(build_variables, "thshield", bldgssizesparam.thshield)
    monkeypatch.setattr(build_variables, "shldith", bldgssizesparam.shldith)
    monkeypatch.setattr(build_variables, "shldoth", bldgssizesparam.shldoth)
    monkeypatch.setattr(build_variables, "scrapli", bldgssizesparam.scrapli)
    monkeypatch.setattr(build_variables, "scraplo", bldgssizesparam.scraplo)
    monkeypatch.setattr(build_variables, "fwith", bldgssizesparam.fwith)
    monkeypatch.setattr(build_variables, "fwoth", bldgssizesparam.fwoth)
    monkeypatch.setattr(build_variables, "blnkith", bldgssizesparam.blnkith)
    monkeypatch.setattr(build_variables, "blnkoth", bldgssizesparam.blnkoth)
    monkeypatch.setattr(build_variables, "r_cp_top", bldgssizesparam.r_cp_top)
    monkeypatch.setattr(divertor_variables, "divfix", bldgssizesparam.divfix)
    monkeypatch.setattr(physics_variables, "rmajor", bldgssizesparam.rmajor)
    monkeypatch.setattr(physics_variables, "rminor", bldgssizesparam.rminor)

    buildings_module.bldgs_sizes(
        tf_radial_dim=bldgssizesparam.tf_radial_dim,
        tf_vertical_dim=bldgssizesparam.tf_vertical_dim,
        outfile=bldgssizesparam.outfile,
        iprint=bldgssizesparam.iprint,
    )

    assert buildings_variables.reactor_hall_l == pytest.approx(
        bldgssizesparam.expected_reactor_hall_l
    )
    assert buildings_variables.reactor_hall_w == pytest.approx(
        bldgssizesparam.expected_reactor_hall_w
    )
    assert buildings_variables.reactor_hall_h == pytest.approx(
        bldgssizesparam.expected_reactor_hall_h
    )
