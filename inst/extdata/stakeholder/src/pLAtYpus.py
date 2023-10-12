
import datetime

from ETS_CookBook import ETS_CookBook as cook

import solver
import maps
import survey_to_pLAtYpus
import process_survey_data


if __name__ == '__main__':
    parameters_file_name = 'C:/Users/liethjs/Documents/gretan/inst/extdata/stakeholder/pLAtYpus.toml'
    parameters = cook.parameters_from_TOML(parameters_file_name)
    parameters['survey']['data']['output']['output_folder'] = 'C:/Users/liethjs/Documents/gretan/inst/extdata/stakeholder/input'
    parameters['survey']['data']['source']['source_folder'] = 'C:/Users/liethjs/Documents/gretan/inst/extdata/stakeholder/input'
    parameters['files']['output_folder'] = 'C:/Users/liethjs/Documents/gretan/inst/extdata/stakeholder/output'
    parameters['maps']['map_data_folder'] = 'C:/Users/liethjs/Documents/gretan/inst/extdata/stakeholder/input/map_data'

    start = datetime.datetime.now()
    print('Needed to add something below the last table')
    print('For business and policmakers to ensure the iteraion is terminated')
    print('Some codes were missing for citizens')
    print('c35_petrol_diesel, c35_hybrid, c_35_electric')
    stakeholders = parameters['pLAtYpus']['stakeholders']
    for stakeholder in stakeholders:
        topic_answers, topic_dataframe = process_survey_data.read_source_sheet(
            stakeholder, parameters
        )
        print(stakeholder)

        process_survey_data.write_full_processed_data(
            parameters, topic_answers, topic_dataframe,
            stakeholder
        )
    end = datetime.datetime.now()
    print((end-start).total_seconds())
    start = datetime.datetime.now()
    survey_to_pLAtYpus.get_survey_product_values(parameters)
    end = datetime.datetime.now()
    print((end-start).total_seconds())
    print('Asymmetric in some cases (say trust)?')
    print('Gas has data only for some countries')
    print('If common, repeat')

    start = datetime.datetime.now()
    solver.get_all_evolutions_and_plots(parameters)
    end = datetime.datetime.now()
    print((end-start).total_seconds())

    start = datetime.datetime.now()
    maps.make_long_term_average_tables(parameters)
    maps.make_area_maps(parameters)
    end = datetime.datetime.now()
    print((end-start).total_seconds())
    print('EU level')
    print('Do weighted survey scores?')
    print('Turn off plotting of survey scores and intention weights for tool')
    print('Can also turn off saving to DB/files/only one format?')
    print('Maybe add country/product filter to speed up?')
