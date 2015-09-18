#include "modelwrapper.hpp"

#include "model.hpp"
#include "generate_response.hpp"
#include "fitmodel_in_CPP.hpp"
#include <Rcpp.h>
//#include <boost/thread.hpp>

int verbosity=0;
bool debug=false;

ModelWrapper::ModelWrapper() : model(NULL), linear_approximation(FALSE) { }

ModelWrapper::~ModelWrapper() {
  if (model!=NULL) delete model;
}

void ModelWrapper::printResponse() {
    if (model == NULL) throw(std::logic_error("Model not set"));
    model-> printResponse();
} 

size_t ModelWrapper::modelRank() {
    if (model == NULL) throw(std::logic_error("Model not set"));
    else return model->modelRank();
    return -1;
} 

size_t ModelWrapper::nr_of_parameters() {
  if (model == NULL) throw(std::logic_error("Model not set"));
  else return model->nr_of_parameters();
  return -1;
} 

// Check if the experiment design is consistent
bool ModelWrapper::model_design_consistent(ExperimentalDesign &exp, ModelStructure &mod) {
  int nodes= mod.getAdjacencyMatrix().shape()[0]; 
  if (nodes!=mod.getAdjacencyMatrix().shape()[1]) 
    throw (std::invalid_argument("adj matrix not square"));
  if (nodes!=mod.getNames().size()) 
    throw (std::invalid_argument("names size wrong"));
  if (exp.basal_activity.size() != nodes) 
    throw (std::invalid_argument("basal activity size wrong"));
  if (exp.inhib_nodes.size() != exp.inhibitor.shape()[1]) 
    throw (std::invalid_argument("inhib_nodes and inhibitor don't match"));
  if (exp.stim_nodes.size() != exp.stimuli.shape()[1]) 
    throw (std::invalid_argument("inhib_nodes and inhibitor don't match"));
  if (exp.stimuli.shape()[0] != exp.inhibitor.shape()[0]) 
    throw (std::invalid_argument("number of stimuli and inhibitors don't match"));
  return true;
}

void ModelWrapper::setModel(ExperimentalDesign exp, ModelStructure mod) {
  model_design_consistent(exp,mod);

  if(debug) {std::cout << mod;} // DEBUGGING  
  generate_response(response_full_model,  
		      symbols_full_model,
		      mod.getAdjacencyMatrix(),
		      exp,
		      mod.getNames());
  adjacency_matrix.resize(boost::extents[mod.getAdjacencyMatrix().shape()[0]]
			  [mod.getAdjacencyMatrix().shape()[1]]);
  adjacency_matrix=mod.getAdjacencyMatrix();
  
  if (model != NULL) delete model;
  model = new Model(response_full_model,  
		    symbols_full_model,
		    exp, linear_approximation);
} 

SEXP ModelWrapper::simulate(Data data, std::vector<double> parameters) {

  if ( model == NULL ) 
    throw std::logic_error("Model not initialized yet. use setModel() before");

  if ( parameters.size() != model->nr_of_parameters() ) 
    throw std::invalid_argument("length of parameter vector invalid");

  double_matrix datax;
  model->predict(parameters, datax, &data);
  Rcpp::List ret;
  ret["prediction"]=datax;

  return ret;
}

SEXP ModelWrapper::fitmodel(Data data, std::vector<double> parameters)  { 

  if ( parameters.size() != model->nr_of_parameters() ) 
    throw std::invalid_argument("length of parameter vector invalid");

    double residual;
    double_matrix  predictions;
    ::fitmodel(parameters, &residual, predictions, model, &data);
    Rcpp::List ret;
    Rcpp::NumericVector pars( parameters.begin(), parameters.end() );
    ret["parameter"]=pars;
    ret["residuals"]=residual;
    return ret;
}

SEXP ModelWrapper::annealingFit(Data data, std::vector<double> parameters, int max_it, int max_depth) {

    if (parameters.size() != model->nr_of_parameters()) {
        parameters.resize(model->nr_of_parameters());
    }
    
    // Find an approximation of the optimum with the simulated annealing
    double residual;
    //std::cerr << "Starting simulated annealing" << std::endl;
    ::simulated_annealing(model, &data, parameters, residual, max_it, max_depth);
    //std::cerr << "really in" << std::endl;

    // Ajust the gradient descent
    double_matrix predictions;
    ::fitmodel(parameters, &residual, predictions, model, &data);
    Rcpp::List ret;
    Rcpp::NumericVector pars( parameters.begin(), parameters.end() );
    ret["parameter"]=pars;
    ret["residuals"]=residual;
    return ret;
}

// Computes the profile likelihood of one parameter and the functionnal relationships of the others with this parameter
SEXP ModelWrapper::profileLikelihood(const Data data, const std::vector<double> parameters, size_t target, const unsigned int total_steps = 10000) {
    if ( parameters.size() != model->nr_of_parameters() ) 
        throw std::invalid_argument("length of parameter vector invalid");

    double param_value = parameters[target-1];
    std::vector<size_t> keep_constant(1, target-1); // -1 for R users
    std::vector< std::vector<double> > residual_track;
    std::vector<double> explored;
    pl_analysis thresholds;
    thresholds.decision = 0.95;

    ::profile_likelihood( data, parameters, keep_constant, residual_track, explored, param_value, model, thresholds, total_steps);
    
    Rcpp::List ret;
    Rcpp::NumericMatrix track(parameters.size(), explored.size());
    for (int i=0 ; i < parameters.size() ; i++) {
        for (int j=0 ; j < explored.size() ; j++) {
            track(i,j) = residual_track[i][j];
        }
    }
    std::vector<std::string> paths;
    model->getParametersLinks(paths);
    std::vector<double> threshold_values; 
    threshold_values.push_back(thresholds.pointwise_threshold);
    threshold_values.push_back(thresholds.simultaneous_threshold);

    // Switch to a C++ array indexing to the R one
    for (size_t i=0 ; i < thresholds.negative_uncertainty.size() ; i++) {
        thresholds.negative_uncertainty[i] += 1;
    }
    for (size_t i=0 ; i < thresholds.positive_uncertainty.size() ; i++) {
        thresholds.positive_uncertainty[i] += 1;
    }

    ret["residuals"] = track;
    ret["explored"] = explored;
    ret["lower_pointwise"] = thresholds.ln_threshold;
    ret["upper_pointwise"] = thresholds.lp_threshold;
    ret["lower_simultaneous"] = thresholds.hn_threshold;
    ret["upper_simultaneous"] = thresholds.hp_threshold;
    ret["thresholds"] = threshold_values;
    ret["path"] = paths[target-1];
    ret["pathid"] = target;
    ret["value"] = param_value;
    ret["lower_error_index"] = thresholds.negative_uncertainty;
    ret["upper_error_index"] = thresholds.positive_uncertainty;

    return ret;

}

/* COMMENTED BECAUSE IT NEEDS rref.hpp which is not in ~/include/fitmodel
 * IF YOU UNCOMMENT THIS, UNCOMMENT THE rref.hpp HEADERS AS WELL
 *
// Returns the profile likelihood and functionnal relationship for all parameters, does the computation in parallel
SEXP ModelWrapper::parallelPL(const Data data, std::vector<double> parameters, const unsigned int total_steps = 10000) {
    if ( parameters.size() != model->nr_of_parameters() ) 
        throw std::invalid_argument("length of parameter vector invalid");

    std::vector< std::vector<size_t> > keep_constant;
    std::vector< std::vector< std::vector<double> > > residual_track;
    std::vector< std::vector<double> > explored;
    std::vector< bool* > identifiability;
    std::vector< std::vector<double> > thresholds;
    std::vector<size_t> target;
    std::vector< std::vector<double> > params;

    // Creation of the threads
    //std::vector<boost::thread*> threads;
    boost::thread_group threads;
    for (int i=0 ; i<parameters.size() ; i++) {
        keep_constant.push_back(std::vector<size_t>(1, i));
        residual_track.push_back(std::vector< std::vector<double> >());
        explored.push_back(std::vector<double>());
        identifiability.push_back(new bool[4]);
        thresholds.push_back(std::vector<double>());
        target.push_back(i);
        params.push_back(parameters);

        // Create a new thread for each parameter
        //threads.add_thread(new boost::thread(::profile_likelihood, data, params[i], keep_constant[i], residual_track[i], explored[i], parameters[i], model, identifiability[i], thresholds[i]));
        // Ugly cast for bind desambiguation        
        //threads.create_thread(boost::bind( static_cast< void(*)(const Data&,std::vector<double>,const std::vector<size_t>,std::vector< std::vector<double> >&,std::vector<double>&,const double,const Model*,bool*,std::vector<double>&)>(::profile_likelihood), data, params[i], keep_constant[i], residual_track[i], explored[i], parameters[i], model, identifiability[i], thresholds[i]));
        void (*pl) (const Data&,std::vector<double>,const std::vector<size_t>,std::vector< std::vector<double> >&,std::vector<double>&,const double,const Model*,bool*,std::vector<double>&) = &::profile_likelihood;
        threads.create_thread(boost::bind( pl, data, params[i], keep_constant[i], residual_track[i], explored[i], parameters[i], model, identifiability[i], thresholds[i]));
        // Without total_steps because it is max 9 arguments plus the function
    }
    threads.join_all();

    Rcpp::List ret(parameters.size());
    Rcpp::List returned;
    std::vector<std::string> paths;
    for (int i=0 ; i<parameters.size() ; i++) {

        model->getParametersLinks(paths);
        Rcpp::NumericMatrix track(parameters.size(), explored.size());
        for (int k=0 ; k < parameters.size() ; k++) {
            for (int j=0 ; j < explored.size() ; j++) {
                track(k, j) = residual_track[i][k][j];
            }
        }

        returned["residuals"] = track;
        returned["explored"] = explored[i];
        returned["lowt_upper"] = identifiability[i][2];
        returned["lowt_lower"] = identifiability[i][0];
        returned["hight_upper"] = identifiability[i][3];
        returned["hight_lower"] = identifiability[i][1];
        returned["thresholds"] = thresholds[i];
        returned["path"] = paths[i];
        returned["pathid"] = target[i];

        ret[to_string(i)] = returned;

        delete[] identifiability[i];
    }

    return ret;
}
*/

SEXP ModelWrapper::getLocalResponse( std::vector<double> p ) {
  
    if ( model == NULL ) 
        throw std::logic_error("Model not initialized yet. use setModel() before");
    if ( p.size() != model->nr_of_parameters() ) 
        throw std::invalid_argument("length of parameter vector invalid");

    double_matrix fit_response_matrix; 
    std::vector<double> fit_inh;
    std::vector<double> p_new;
    {
      model->convert_identifiables_to_original_parameter( p_new, p );
      Model::convert_original_parameter_to_response_matrix( fit_response_matrix, fit_inh, p_new, adjacency_matrix );
    }
    Rcpp::List ret;
    ret["local_response"]=fit_response_matrix;
    ret["inhibitors"]=fit_inh;
    return ret;
    
}

std::vector<double> ModelWrapper::getParameterFromLocalResponse( const double_matrix &response, const std::vector<double> inhib) {
  
  if ( model == NULL ) 
    throw std::logic_error("Model not initialized yet. use setModel() before");
  
  std::vector<double> parameter;
  std::vector<double> tmpp;
  
  Model::convert_response_matrix_to_original_parameter( tmpp, response, 
							inhib, adjacency_matrix );
  model->convert_original_parameter_into_identifiable(parameter,tmpp);

  return parameter;
  
}

// Gives the links combination for each identifiable response coefficient
SEXP ModelWrapper::getParametersLinks() {

    std::vector<std::string> buf_string;

    model->getParametersLinks(buf_string);
    Rcpp::CharacterVector ret(buf_string.size());
    for (int i=0 ; i < buf_string.size() ; i++) {
        ret(i) = buf_string[i];
    }
   
    return ret;
}

// Show the parameter dependency matrix G before and after reduction
void ModelWrapper::showParameterDependencyMatrix() {
    model->showParameterDependencyMatrix();
}

void ModelWrapper::showGUnreduced() {
    model->showGUnreduced();
}

SEXP ModelWrapper::getUnreducedPDM() {
    std::vector< std::vector<int> > Gu = model->getUnreducedParameterDependencyMatrix();
    Rcpp::NumericMatrix Gunreduced(Gu.size(), Gu[0].size());
    for (int i=0 ; i < Gu.size() ; i++) {
        for (int j=0 ; j < Gu[0].size() ; j++) {
            Gunreduced(i,j) = Gu[i][j];
        }
    }
    return Gunreduced;
}

SEXP ModelWrapper::getParametersNames() {
    std::vector<std::string> tmp;
    model->getParametersLinks(tmp);
    Rcpp::List ret;
    ret["names"]=tmp;

    return ret;
}

// Converts R index to C++ index
void ModelWrapper::printEquation(const size_t r, const size_t c) {
    model->printEquation(r-1, c-1);
}


RCPP_MODULE(ModelEx) {
  using namespace Rcpp ;
  class_<ModelWrapper>( "Model" )
    .default_constructor()
    .method( "setModel", &ModelWrapper::setModel )
    .method( "simulate", &ModelWrapper::simulate )

    .method( "printResponse", &ModelWrapper::printResponse )
    .method( "modelRank", &ModelWrapper::modelRank )
    .method( "nr_of_parameters", &ModelWrapper::nr_of_parameters )
    .method( "fitmodel", &ModelWrapper::fitmodel )
    .method( "annealingFit", &ModelWrapper::annealingFit )
    .method( "getLocalResponseFromParameter", &ModelWrapper::getLocalResponse )
    .method( "getParameterFromLocalResponse", &ModelWrapper::getParameterFromLocalResponse )
    .method( "profileLikelihood", &ModelWrapper::profileLikelihood)
    //.method( "parallelPL", &parallelPL )
    .method( "getParametersLinks", &ModelWrapper::getParametersLinks )
    .method( "showPDM", &ModelWrapper::showParameterDependencyMatrix )
    .method( "showUnreducedPDM", &ModelWrapper::showGUnreduced )
    .method( "getParametersNames", &ModelWrapper::getParametersNames )
    .method( "getUnreducedPDM", &ModelWrapper::getUnreducedPDM )
    .method( "getEquation", &ModelWrapper::printEquation )
    .field("linear_approximation", &ModelWrapper::linear_approximation, "Linear Approximation" )
    ;
}
