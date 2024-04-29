#' @export
new_db.mongodb <- function(driver, db, url, ...) {
  # driver checkers
  stopifnot(requireNamespace("mongolite"))
  data <- list(
    collections = mongolite::mongo(
      collection = "collections",
      db = db,
      url = url, ...
    ),
    items = mongolite::mongo(
      collection = "items",
      db = db,
      url = url, ...
    )
  )
  structure(data, class = driver[[1]])
}

#' @export
db_collections_id_exist.mongodb <- function(db, ids) {
  # print('db_collections_id_exist.mongodb')
  result <- mongo_collections(db, mongo_in("id", ids))
  collections_id <- sapply(result, function(x) x$id )
  ids %in% collections_id
}

#' @export
db_collections.mongodb <- function(db) {
  # print('db_collections.mongodb')
  unname(db$collections)
}

#' @export
db_collection.mongodb <- function(db, collection_id) {
  # print('db_collection.mongodb')
  mongo_collections(db, mongo_equal("id",collection_id))[[1]]
}

#' @export
db_items_id_exist.mongodb <- function(db, collection_id, ids) {
  # print('db_items_id_exist.mongodb')
  items <- mongo_items(db, mongo_and(mongo_in("id", ids), mongo_in("collection", collection_id)))
  items_id <- sapply(items$features, function(x) x$id )
  ids %in% items_id

}

#' @export
db_items.mongodb <- function(db, collection_id, limit, bbox, datetime, page) {
  # print('db_items.mongodb ')
  items <- mongo_items(db, mongo_equal("collection", collection_id))
  items
  # datetime filter...
  # exact_date <- get_datetime_exact(datetime)
  # start_date <- get_datetime_start(datetime)
  # end_date <- get_datetime_end(datetime)
  # # ...exact_date
  # if (!is.null(exact_date)) {
  #   items <- mongo_filter_exact_date(items, exact_date)
  # } else {
  #   # ...start_date
  #   if (!is.null(start_date))
  #     items <- mongo_filter_start_date(items, start_date)
  #   # ...end_date
  #   if (!is.null(end_date))
  #     items <- mongo_filter_end_date(items, end_date)
  # }
  # # spatial filter
  # if (!is.null(bbox)) {
  #   items <- mongo_filter_spatial(items, bbox_as_polygon(bbox))
  # }
  # items$numberMatched <- length(items$features)
  # # manage pagination
  # mongo_paginate_items(items, limit, page)
}

#' @export
db_item.mongodb <- function(db, collection_id, item_id) {
  # print('db_item.mongodb')
  item <- mongo_items(
    db,
    mongo_and(
      mongo_equal('collection', collection_id),
      mongo_equal('id', item_id)
    )
  )

  class(item) <- c("doc_item", "list")
  item
}

#' @export
db_search.mongodb <- function(db,
                              limit,
                              bbox,
                              datetime,
                              intersects,
                              ids,
                              collections,
                              page) {
  # print('db_search.mongodb')
  #
  # a <- db$items$find(query=mongo_in('collection',collections))
  #
  #
  # items <- mongo_search_items(features)
  # items$numberMatched <- length(items$features)
  # # manage pagination
  # mongo_paginate_items(items, limit, page)
}

mongo_new_items <- function(features) {
  # print('mongo_new_items')
  structure(list(
    type = "FeatureCollection",
    features = features
  ), class = c("doc_items", "list"))
}



mongo_items_id <- function(items) {
  # print('mongo_items_id')
  rstac::items_reap(items, "id")
}

mongo_items_datetime <- function(items) {
  # print('mongo_items_datetime')
  as.Date(rstac::items_datetime(items))
}

mongo_filter_ids <- function(items, ids) {
  # print('mongo_filter_ids')
  select <- which(mongo_items_id(items) %in% ids)
  items$features <- items$features[select]
  items
}

mongo_filter_exact_date <- function(items, exact_date) {
  # print('mongo_filter_exact_date')
  select <- mongo_items_datetime(items) == as.Date(exact_date)
  items$features <- items$features[select]
  items
}

mongo_filter_start_date <- function(items, start_date) {
  # print('mongo_filter_start_date')
  select <- mongo_items_datetime(items) >= as.Date(start_date)
  items$features <- items$features[select]
  items
}

mongo_filter_end_date <- function(items, end_date) {
  # print('mongo_filter_end_date')
  select <- mongo_items_datetime(items) <= as.Date(end_date)
  items$features <- items$features[select]
  items
}

mongo_filter_spatial <- function(items, geom) {
  # print('mongo_filter_spatial')
  select <- rstac::items_intersects(items, geom)
  items$features <- items$features[select]
  items
}

mongo_paginate_items <- function(items, limit, page) {
  # print('mongo_paginate_items')
  if (is.null(limit)) limit <- 10
  if (is.null(page)) page <- 1
  pages <- get_pages(items, limit)
  if (pages > 0) {
    api_stopifnot(
      page <= pages,
      status = 400,
      "page not less than or equal to ", pages
    )
    # select page items
    index_from <- (page - 1) * limit + 1
    index_to <- if (page == pages) {
      length(items$features)
    } else {
      page * limit
    }
    select <- seq(index_from, index_to)
    items$features <- items$features[select]
  }
  items$numberReturned <- length(items$features)
  items
}

mongo_list <- function(values) {
  UseMethod("mongo_list", values)
}

#' @export
mongo_list.numeric <- function(values) {
  paste0(values, collapse = ",")
}

#' @export
mongo_list.default <- function(values) {
  paste0('"', values, '"', collapse = ",")
}

list_to_item <- function(item) {
  structure(item, class = c("doc_item", "rstac_doc","list"))
}

list_to_collection <- function(collection) {
  structure(collection, class = c("doc_collection", "rstac_doc","list"))
}

mongo_items <- function(db, query, limit=0, page=0) {
  skip = (page - 1) * limit
  result <- db$items$iterate(
    query = jsonlite::toJSON(query, auto_unbox = TRUE),
    skip = skip, limit = limit
  )
  items <- list()
  x <- result$one()
  while (!is.null(x)) {
    item  <- list_to_item(x)
    items <- append(items, list(item))
    x <- result$one()
  }
  structure(
    list(
      type="FeatureCollection",
      features=items,
      links = list()
    ),
    class=c("doc_items", "rstac_doc","list")
  )
}

mongo_collections <- function(db, query) {
  result <- db$collections$iterate(query=jsonlite::toJSON(query, auto_unbox = TRUE))
  collections <- list()
  while(!is.null(x <- result$one())){
    collection  <- list_to_collection(x)
    collections <- append(collections, list(collection))
  }
  structure(
    collections,
    class=c("doc_collections", "rstac_doc","list")
  )
}

# ---- operators ----

mongo_in <- function(field, values) {
  query <- list(list("$in"=as.list(values)))
  names(query) = field
  query
}

mongo_and <- function(expr1, expr2) {
  utils::modifyList(expr1, expr2)
}
mongo_equal <- function(field, value) {
  query <- list(list("$eq"=value))
  names(query) = field
  query
}
