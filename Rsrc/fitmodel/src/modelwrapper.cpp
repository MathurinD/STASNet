#include "modelwrapper.hpp"

#include <fitmodel/model.hpp>
#include <fitmodel/generate_response.hpp>
#include <fitmodel/fitmodel_in_CPP.hpp>
#include <Rcpp.h>

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

  
bool ModelWrapper::model_design_consistent(ExperimentalDesign &exp, ModelStructure &mod) {
  int nodes= mod.getAdjacencyMatrix().shape()[0]; 
  if (nodes!=mod.getAdjacencyMatrix().shape()[0]) 
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
  model->predict(&(parameters[0]), datax, &data);
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

// Computes the profile likelihood for one parameters
// The code might be burried deeper into the C++ code in fitmodel_in_CPP
// SHOULD we extend ot to all to allow minimum work in the R code
SEXP ModelWrapper::profileLikelihood(Data data, std::vector<double> parameters, size_t target, const unsigned int total_steps = 10000, const double step_size = 0.01) {
    if ( parameters.size() != model->nr_of_parameters() ) 
        throw std::invalid_argument("length of parameter vector invalid");

        double param_value = parameters[target-1]
        double residual;
        double_matrix prediction; // Ask Nils what it does
        std::vector<size_t> keep_constant(1, target-1); // -1 for R users
        std::vector<double> residual_track();
        std::vector<double> explored();

        // Initial fit
        ::fitmodel(parameters, &residual, prediction, model, &data);

        // Thresholds
        bool pointwise_thr = residual + boost::math::cdf( boost::math::chi_squared(1) );
        bool simultaneous_thr = residual + boost::math::cdf( boost::math::chi_squared(parameters.size()) );
        
        double scanned_value = param_value - total_steps * step_size / 2;
        bool struct_n_identifiability = true;
        bool pract_n_identifiability = true;
        for (unsigned int i=0 ; i < total_steps ; i++) {
            parameters[target-1] = scanned_value;
            scanned_value += step_size;

            ::fitmodel(parameters, &residual, prediction, model, &data, keep_constant);
            explored.push_back(scanned_value);
            residual_track.push_back(residual);

            if (residual > pointwise_thr) struct_n_identifiability = false;
            if (residual > simultaneous_thr) pract_n_identifiability = false;
        }

        Rcpp::List ret;
        Rcpp::NumericVector track( residual_track.begin(), residual_track.end() );
        ret["residuals"] = residual_track;
        ret["scanned"] = explored;
        ret["structural"] = struct_n_identifiability;
        ret["practical"] = pract_n_identifiability;

        return ret;

}

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
    .method( "getLocalResponseFromParameter", &ModelWrapper::getLocalResponse )
    .method( "getParameterFromLocalResponse", &ModelWrapper::getParameterFromLocalResponse )
    .field("linear_approximation", &ModelWrapper::linear_approximation, "Linear Approximation" )
    ;
}

int verbosity=0;
