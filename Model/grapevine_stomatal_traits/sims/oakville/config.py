from datetime import datetime
from enum import Enum

from grapevine_stomatal_traits.sources.config import SiteData, PhenoData


class SiteDataOakville(SiteData):
    def __init__(self, pheno_data: PhenoData):
        super(SiteDataOakville, self).__init__(
            pheno_data=pheno_data,
            latitude=38.43,
            longitude=-122.41,
            elevation=58,
            training_system='vsp',
            soil_class='Clay_Loam',
            spacing_interrow=2.1,
            spacing_intrarow=1.8,
            soil_depth=2.,
            rhyzo_coeff=0.65,
            avg_root_radius=0.001,
            root_length=1000.,
            initial_soil_water_potential=-0.6)


ScenariosDatesOakville = [
    ('historical', PhenoData(
        date_budburst=datetime(1990, 4, 1),
        date_veraison=datetime(1990, 7, 30),
        gdd_since_budbreak=894)),
    ('rcp45', PhenoData(
        date_budburst=datetime(1990, 3, 30),
        date_veraison=datetime(1990, 7, 16),
        gdd_since_budbreak=1019)),
    ('rcp85', PhenoData(
        date_budburst=datetime(1990, 3, 27),
        date_veraison=datetime(1990, 7, 6),
        gdd_since_budbreak=1087))]
