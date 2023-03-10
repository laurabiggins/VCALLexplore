#' R6 Class
#'
#' @description
#' A VCall contains a set of Vcalls and various other data fields.
#'
#' @details
#' Some more info here.

VCall <- R6::R6Class("VCall", 
  public = list(
  
    #' @field name Name of the dataset.
    name = NULL,
    
    #' @field J_calls The J calls. Dataset containing the columns:
    #' J_CALL, V_CALL, n, percentage, to add: V_GROUP, RF, CDR3_length
    J_calls = NA,
    D_calls = NA,
    V_calls = NA,
    np_lengths = NA,
    aa_lengths = NA,
    aa_counts_left = NA,
    
    initialize = function(
      name, 
      J_calls=NA, 
      D_calls=NA, 
      V_calls = NA,
      np_lengths = NA, 
      aa_lengths = NA,
      aa_counts_left = NA
    ) {
      
      stopifnot(is.character(name), length(name) == 1)
      
      self$name <- name
      self$J_calls <- J_calls
      self$D_calls <- D_calls
      self$V_calls = V_calls
      self$np_lengths <- np_lengths
      self$aa_lengths <- aa_lengths
      self$aa_counts_left <- aa_counts_left
    },
    
    get_Jcalls = function(v_call, vgroup = FALSE) {
      
      if(group == TRUE){
        
        
      }
      else {
        if(v_call %in% self$J_calls$V_CALL) {
          return(dplyr::filter(self$J_calls, V_CALL==v_call))
        }
      }
    },
    
    get_Dcalls = function(v_call) {
      if(v_call %in% self$D_calls$V_CALL) {
        dplyr::filter(self$D_calls, V_CALL==v_call)
      }
    },
    
    get_np_lengths= function(v_call) {
      if(v_call %in% self$np_lengths$V_CALL) {
        dplyr::filter(self$np_lengths, V_CALL==v_call)
      }
    },
    
    get_aa_lengths= function(v_call) {
      if(v_call %in% self$aa_lengths$V_CALL) {
        dplyr::filter(self$aa_lengths, V_CALL==v_call)
      }
    },
    
    get_aa_counts_left = function(v_call) {
      if(v_call %in% self$aa_counts_left$V_CALL) {
        dplyr::filter(self$aa_counts_left, V_CALL==v_call)
      }
    }
  
))