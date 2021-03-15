library(raster)
library(dplyr)
library(magrittr)
library(data.table)

## Chess board equivalent representation in raster
## Inspect the raster object and think about how we could combine them for masking multi-piece patterns
## https://rdrr.io/cran/raster/man/raster-package.html
## https://rspatial.org/raster/pkg/2-classes.html

## raster is a virtual class, with inheriting classes RasterLayer, RasterStack, RasterBrick
## So multi-variable chess data-structure, we can conceive of a raster object manipulated for actual implementation of gameplay.
## No reinventing the wheel, we should probably use bigchess to keep the gamestate in a PGN format.
## Hopefully from this state we can make a fast way to derive the initial raster representation from PGN and
## methods on the raster class will manage updating positions and derived features.
## with a raster layer as a basic unit for 8x8 one-hot parameter encoding of some relationship of interest
## we can maintain a number of human-interpretable "positional' views of gamestate possibilities.
  ## Given that I must make a move, out of my possible moves, 
  ## what do I think about the opportunities that will create for me and my opponent?
  ## Ideally reinforcement learning through self-play would be used,
  ## and we can observe if it plays like a human, given our instantiation of an axiomatic human strategy.

## We want to parameterize the model to be able to pick up on patterns in the pieces.
## ML on raster states throughout the game is the idea. 

board <- raster(xmn = 0, xmx = 8, 
               ymn = 0, ymx = 8, 
               nrows = 8, ncols = 8)

glimpse(board)
board[] = 1:64
plot(board)

## start with {a1, b1, ..., a8, b8, ..., g8, h8}
## must be careful about how we set up matrix notation
board_positions = outer(letters[1:8], 1:8, FUN = paste0)
str(list(board_positions))

## Natural outer product orientation is not chess orientation
board_positions

## Transpose + horizontal flip to get chess orientation relative to file/rank
t(outer(letters[1:8], 8:1, FUN = paste0))

## This is white's view of the board.
## These combined traansform the natural index to white's view.
white_matrix =  t(outer(letters[1:8], 8:1, FUN = paste0))

## black's view of the board is a 180 degree rotation, just rev()
black_matrix = matrix(rev(white_matrix), nrow = 8)

## Natural index is normal matrix notation, small numbers/letters at the top left
## Also natural with respect to R's default column-by-column matrix default
## Increase indices as you go down/right
## Since positions are referred to as "file-rank", then use that for "row-column" coordinates
## I will align this with a 1-64 index based on default matrix() by-column ordering.
## So we will list a1, b1, ..., f1, a2, b2,... and this order will be default processed by matrix correctly
natural_matrix = matrix(outer(letters[1:8], 1:8, FUN = paste0), nrow = 8)
natural_index = as.character(natural_matrix)

## Lets make an object to have tidy indices
ind_table = data.table(pos = natural_index)
ind_table[, c("file", "rank") := tstrsplit(pos,"")]

ind_table[, natural := lapply(pos, function(x) {which(natural_index == x)})]

head(ind_table)

ind_table[, white := sapply(pos,
                            function(x) {which(white_matrix == x)})]
head(ind_table)

## Verifying mapping: the matrix defined by the order of white indices matches the white matrix
identical(matrix(ind_table[order(white),pos], nrow = 8), white_matrix)

ind_table[, black := sapply(pos,
                            function(x) {which(black_matrix == x)})]

identical(matrix(ind_table[order(black),pos], nrow = 8), black_matrix)

head(ind_table)


## Helper functions for translating character positions
get_natural <- function(chr) {
  return(as.numeric(ind_table[pos == chr, natural]))
}

get_white <- function(chr) {
  return(as.numeric(ind_table[pos == chr, white]))
}

get_black <- function(chr) {
  return(as.numeric(ind_table[pos == chr, black]))
}

get_natural("a8")
get_white("a8")
get_black("a8")

## Helper function to generate positions based on numeric equivalent
## A-H = 1-8 for file, 1-8 for rank (i.e., white perspective)
chr_pos <- function(file, rank) {
  return(paste0(letters[file], rank))
}
chr_pos(2,2)
## Position encoder - make a matrix with indicator at one index
get_matrix <- function(indx) {
  tmp = matrix(rep(0,64), nrow = 8)
  tmp[indx] = 1
  return(tmp)
}

## Ta-da!
get_matrix(get_white("a1"))
get_matrix(get_black("a1"))
get_matrix(get_white(chr_pos(5,4)))
get_matrix(get_natural("a5"))

## Ok -- now what would a chess state look like?
## We are using this structure to represent each piece's possibilities in an easy-to-work-with way
## Intuition, build a raster layer for each individual piece. Initialize a starting board state

## For simplicity, I would like the raster to store objects in the natural matrix indexing
### Why: easy intuitive lookup for positions, f7 is the 6th letter 7th number, 
###       so 6th row 7th column would follow our matrix intuitions for order of providing coordinates.
## and when accessing, I can mask with white/black POV.

## What all would we need to full define a starting state?
## More helpers first
get_file <- function(num) {
  return(
    paste0(letters[num], 1:8))
}

get_rank <- function(num) {
  return(
    paste0(letters[1:8], num))
}

pieces = c("pawn", "knight", "bishop", "rook", "queen", "king")
n_pieces = c(8, 2, 2, 2, 1, 1)

initial_state_white = list(id = 1:16,
  piece = unlist(sapply(1:6, function(i){c(rep(pieces[i],n_pieces[i]))})),
  locs = c(get_rank(2), paste0(c("b","g", ## knight 1,2
                                       "c", "f", ## bishop 1,2
                                       "a", "h", ## rook 1,2
                                       "d", # queen
                                       "e"), # king
                                     1)),
  white_turn = TRUE
)

initial_state_black = list(id = 1:16,
   piece = unlist(sapply(1:6, function(i){c(rep(pieces[i],n_pieces[i]))})),
   locs = c(get_rank(7), paste0(c("b","g", ## knight 1,2
                                        "c", "f", ## bishop 1,2
                                        "a", "h", ## rook 1,2
                                        "d", # queen
                                        "e"), # king
                                      9)),
   white_turn = TRUE
   )

initial_state_white


## Now finally we can construct a meaningful raster object
indicator_grids = lapply(initial_state_white$locs, FUN = function(x) {get_matrix(get_white(x))})

## testing functionality
tmp = raster(xmn = 0, xmx = 8, 
             ymn = 0, ymx = 8, 
             nrows = 8, ncols = 8)
values(tmp) = get_matrix(get_white("a2"))
tmp
plot(tmp)
rm(tmp)

## now apply across all pieces in the starting position
piece_grids = list()
piece_names = paste0(initial_state_white$piece,"_",initial_state_white$locs)

piece_grids = lapply(seq_along(piece_names), 
       function(x) {
         piece_grids[[piece_names[x]]] =
          indicator_grids[[x]]})

piece_rasters = list()

## Testing matrix input default parsing
piece_grids[[1]] ## Pawn on a2
tmp = raster(piece_grids[[1]])
tmp@data@values

## When retrieving values from raster, MUST coerce to matrix with byrow
## I was using by column in my earlier derivation of indices..
tmp@data@values
as.vector(matrix(tmp@data@values, nrow = tmp@nrows, byrow = TRUE))

## Conversely, just transpose. However, it would be nice to have everything on the same basis.
t(matrix(tmp@data@values, nrow = tmp@nrows))

## Alternatively, when we create raster objects, we can just feed it the transpose
## Then, when we reference data in the rasters, it will match our index expectations.
## I like this method better actually.
tmp = raster(t(piece_grids[[1]]))
matrix(tmp@data@values, nrow = tmp@nrows)


## Now construct the rasters for each piece
lapply(seq_along(piece_names),
       function(x) {
         piece_rasters[[piece_names[x]]] =
           raster(piece_grids[[x]])
       })      

