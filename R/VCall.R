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
    V_groups = NA,
    np_lengths = NA,
    aa_lengths = NA,
    aa_counts_left = NA,
    
    initialize = function(
      name, 
      J_calls=NA, 
      D_calls=NA, 
      V_calls = NA,
      V_groups = NA,
      np_lengths = NA, 
      aa_lengths = NA,
      aa_counts_left = NA
    ) {
      
      stopifnot(is.character(name), length(name) == 1)
      
      self$name <- name
      self$J_calls <- J_calls
      self$D_calls <- D_calls
      self$V_calls <- V_calls
      self$V_groups <- V_groups
      self$np_lengths <- np_lengths
      self$aa_lengths <- aa_lengths
      self$aa_counts_left <- aa_counts_left
    },
    
    # default option needs to be to return info for all the vcalls
    # OR vgroup, or vcall if vcall and vgroup are supplied, vcall will take priority and vgroup will be ignored.
    # then select RF
    # then select cdr3 length
    # I don't know if this will get too unwieldy, we'll see I guess.
    # can probably make these functions generic if they're going to be the same for D and J etc.
    
    get_Jcalls = function(v_call = NULL, vgroup = NULL, drf = NULL, CDR3_length = NULL) {
    
      # First filter by vcall or vgroup if supplied
      if (!is.null(v_call)){
        if(v_call %in% self$J_calls$V_CALL) {
          filtJ <- dplyr::filter(self$J_calls, V_CALL==v_call)
        } else {
            warning("Couldn't find specified v_call")
            return(NULL)  
        }
      } else if (!is.null(vgroup)){
          if(vgroup %in% self$J_calls$Vgroup) {
            filtJ <- dplyr::filter(self$J_calls, Vgroup==vgroup)
          } else {
              warning("Couldn't find specified vgroup")
              return(NULL)  
          }
      } else {
        filtJ <- self$J_calls
        #browser()
      }
      # Second - filter by DRF
      if(!is.null(drf)){
          filtJ <- dplyr::filter(filtJ, DRF==drf) %>%
            drop_na(DRF)
      } else {
        # remove the drf column and 
      } 
      if(!is.null(CDR3_length)){
        filtJ <- dplyr::filter(filtJ, CDR3_LENGTH==CDR3_length) %>%
          drop_na(CDR3_LENGTH)
      }  
      filtJ
    },
    # get_total_Jcounts = function() {
    #   
    #   
    # }
    
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