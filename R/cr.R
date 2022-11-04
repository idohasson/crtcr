library(vctrs)


new_clone <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = "clone")
}

clone <- function(x = character()) {
  x <- vec_cast(x, character())
  new_clone(x)
}

is_clone <- function(x) {
  inherits(x, "clone")
}

as_clone <- function(x, ...) {
  UseMethod("as_clone")
}

as_clone.default <- function(x, ...) {
  vec_cast(x, new_clone())
}

as_clone.character <- function(x) {
  value <- as.character(toupper(x))
  new_clone(value)
}

clone_gen <- function() {
  
  coding_seq <- function(n_codons) {
    
    coding_codon <- function() {
      codon <- paste(sample(c("A","G","T","C"), 3, rep = TRUE), collapse = "")
      ifelse(codon %in% c("TGA","TAA","TAG"), coding_codon(), codon)
    }
    paste(replicate(n_codons, coding_codon()), collapse = "")
  }
  Vectorize(coding_seq, "n_codons")
}

translate <- function(nt_vec) {
  # converts the nucleotide vector to uppercase
  # nt_vec <- toupper(nt_vec)
  # checks that each element in the vector is a DNA base.
  stopifnot(all(grepl(pattern = "^[AGTC]+$", nt_vec)))
  
  # a vector of integers that correspond to the nucleotide characters.
  encoding <- c(T = 0, C = 1, A = 2, G = 3)
  # a vector of characters that correspond to the amino acid characters.
  decoding <- strsplit("FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG", "")[[1]]
  # function for converting a DNA character vector into an amino acid character vector.
  nt_to_aa <- function(nt) decoding[encoding[nt[seq(1, length(nt) ,3)]] * 16 +
                                      encoding[nt[seq(2, length(nt) ,3)]] * 4 +
                                      encoding[nt[seq(3, length(nt) ,3)]] + 1]
  
  # splits the nucleotide vector into a character vector of individual nucleotides
  nt_vec <- strsplit(nt_vec, split = "")
    # converts each nucleotide character to an amino acid character.
  aa_vec <- lapply(nt_vec, nt_to_aa)
    # pastes the amino acid characters together into a single string.
  sapply(aa_vec, paste, collapse="")
}

rand_clone <- clone_gen()

rand_rep <- function(n, mu=3) rand_clone(rpois(n, mu)+1)

# as.cha(translate(rand_rep(5)))

# as_clone(translate(rand_rep(5)))
nt=rand_rep(10)
ntl=rpois(10, 10) %>% lapply(rand_rep, mu=1)
df=rand_rep(10)   %>% cbind.data.frame(clone=., clonotype=translate(.))
dfl=rpois(10, 10) %>% lapply(. %>% rand_rep(., mu=1) %>% 
                      cbind.data.frame(clone=., clonotype=translate(.)))
dfg=rpois(10, 10) %>% lapply(. %>% rand_rep(., mu=1) %>% 
                      cbind.data.frame(clone=., clonotype=translate(.))) %>% 
                      setNames(which(!have_name(.))) %>% 
                      bind_rows(.id = "rep_id") 


                        #### NT input ####
  
                      #### To clone DF ####
nt %>% unique %>% 
       cbind.data.frame(clone=., clonotype=translate(.))
                    #### To CR-level table ####
nt %>% unique %>% 
       translate %>% table %>% 
       as.data.frame(responseName="CRlvl") %>% 
       rename(clonotype=".")



                        #### NT list input ####

                        #### To clone DF ####
ntl %>% lapply(. %>% unique %>% 
               cbind.data.frame(clone=., clonotype=translate(.)))

                    #### To CR-level table list ####
ntl %>% lapply(. %>% unique %>% 
               translate %>% table %>% 
               as.data.frame(responseName="CRlvl") %>% 
               rename(clonotype="."))


                          #### DF input ####

                        #### To clone DF ####
df %>% unique()
                      #### To CR-level table ####
df %>% unique() %>% with(table(clonotype)) %>% 
       as.data.frame(responseName ="CRlvl")


                            #### DF list input ####

                          #### To clone-DF list ####
dfl %>% lapply(. %>% unique)
                      #### To clone clone-group-DF ####
dfl %>% lapply(unique) %>% 
   setNames(which(!have_name(.))) %>%
   bind_rows(.id = "rep_id")

                      #### To group-CR-level table ####
x <- list(dfl[1:5], dfl[6:10]) %>% 
  lapply(bind_rows, .id = "rep_id") %>% 
  bind_rows(.id = "group") %$% 
  tapply(clone, cbind.data.frame(group, rep_id, clonotype), n_distinct) %>% 
  ftable(row.vars = "clonotype")

                          #### To averaged-CR ####
library(magrittr)
dfl %>% lapply(unique) %>% 
        setNames(which(!have_name(.))) %>%
        bind_rows(.id = "rep_id") %>% 
        with(table(clonotype, rep_id))
  
  # %$% 
  #       xtabs(formula = CRlvl ~ clonotype + rep_id)
                # tapply(CRlvl, rep_id, mean)
                # tapply(CRlvl, clonotype, mean)


                    #### To share-level table ####
list(dfl[1:5], dfl[6:10]) %>% 
  setNames(which(!have_name(.))) %>%
  lapply(bind_rows, .id = "rep_id") %>% 
  bind_rows(.id = "group") %>%  
  with(table(clonotype, group))

  # bind_rows(.id = "group") %$% 
  # tapply(rep_id, cbind.data.frame(clonotype, group), n_distinct)
  # tapply(group, cbind.data.frame(clonotype, rep_id), n_distinct)
  
dfl %>% lapply(. %>% unique %>% with(table(clonotype)) %>%
                as.data.frame(responseName ="CRlvl")) %>%
                bind_rows() %$% tapply(CRlvl, clonotype, length)
                      #### To group-share-level table ####
dfl %>% lapply(unique) %>% 
        setNames(which(!have_name(.))) %>% 
        bind_rows(.id = "rep_id") %>% 
        with(table(clonotype)) %>% 
        as.data.frame() %>% 
  filter(clonotype == "S")



                          ### group DF input ####

                    ### To clone-group-DF ####
dfg %>% unique()
                #### To share-level table ####
dfg %>% unique() %>% 
        with(table(clonotype, rep_id))



 












