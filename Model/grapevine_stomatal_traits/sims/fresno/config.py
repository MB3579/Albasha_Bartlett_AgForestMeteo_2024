from datetime import datetime
from enum import Enum

from grapevine_stomatal_traits.sources.config import SiteData, PhenoData


class SiteDataFresno(SiteData):
    def __init__(self, pheno_data: PhenoData):
        super(SiteDataFresno, self).__init__(
            pheno_data=pheno_data,
            latitude=36.82,
            longitude=-119.74,
            elevation=104,
            training_system='sprawl',
            soil_class='Clay_Loam',
            spacing_interrow=3.0,
            spacing_intrarow=2.2,
            soil_depth=2.,
            rhyzo_coeff=0.65,
            avg_root_radius=0.001,
            root_length=3000.,
            initial_soil_water_potential=-0.6)


#ScenariosDatesFresno = [
#    ('historical', PhenoData(
#        date_budburst=datetime(1990, 3, 25),
#        date_veraison=datetime(1990, 7, 15),
#        gdd_since_budbreak=1112)),
#    ('rcp45', PhenoData(
#        date_budburst=datetime(1990, 3, 23),
#        date_veraison=datetime(1990, 7, 2),
#        gdd_since_budbreak=1242)),
#    ('rcp85', PhenoData(
#        date_budburst=datetime(1990, 3, 21),
#        date_veraison=datetime(1990, 6, 25),
#        gdd_since_budbreak=1295))]


ScenariosDatesFresno = [
    ('rcp45', PhenoData(
        date_budburst=datetime(1990, 3, 23),
        date_veraison=datetime(1990, 7, 2),
        gdd_since_budbreak=1242)),
    ('rcp85', PhenoData(
        date_budburst=datetime(1990, 3, 21),
        date_veraison=datetime(1990, 6, 25),
        gdd_since_budbreak=1295))]
