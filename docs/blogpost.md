# A Basic Full Text Search Server in Erlang

### by Ward Bekker <ward@tty.nl>

This article explains how to build a basic full text search server in Erlang. The server has the following features:

- indexing
- stemming
- ranking
- faceting
- asynchronous search results
- web frontend using websockets

Familiarity with the [OTP design principles](http://www.erlang.org/doc/design_principles/des_princ.html) is recommended.

The sample application (build with help from my colleague Michel Rijnders <mies@tty.nl>) uses the [Creative Commons Data Dump from StackExchange](http://blog.stackoverflow.com/category/cc-wiki-dump/) as demo data.

We cover the following subjects:

- [running the sample application](#sample_application)
- [OTP supervision tree](#otp_tree)
- [importing demo data](#data_import)
- [indexing](#indexing)
- [stemming](#stemming)
- [faceting](#faceting)
- [querying and relevance ranking](#query_and_ranking)
- [displaying search results](#display_search)
- [improvements](#improvements)

<a name="sample_application" ></a>
## Running the Sample Application

Clone the source from GitHub:

     git clone git://github.com/tty/async_search.git

And start the application:

    $ rebar get-deps compile && erl -pa `pwd`/ebin `pwd`/deps/*/ebin +P 134217727
    Eshell> application:start(async).
    Eshell> stackoverflow_importer_ser:import().

Visit [http://localhost:3000](http://localhost:3000), you should see the following page:

<img src="https://img.skitch.com/20110909-f3i2aiuby9ht1sjh1j5yt42f7x.jpg" alt="http://localhost:3000/" />

Sample ranked search output for the query `erlang armstrong`:

<div class="thumbnail"><a href="https://skitch.com/wardbekker/fas4h/http-localhost-3000"><img src="https://img.skitch.com/20110909-efekgnjk3hwuuhpea5dq5gi48m.preview.jpg" alt="http://localhost:3000/" /></a></div>

Sample tags facets output for the query `java`:

<div class="thumbnail"><a href="https://skitch.com/wardbekker/fas51/http-localhost-3000"><img src="https://img.skitch.com/20110909-ed4e8kcaenbn4bkh6ddt342ig2.preview.jpg" alt="http://localhost:3000/" /></a></div>

<a name="otp_tree" ></a>
## OTP Supervision Tree

<div class="thumbnail"><a href="https://skitch.com/wardbekker/f2rki/supervisor-tree"><img style="max-width:638px" src="https://img.skitch.com/20110911-kcd3i2gexishp7e92m3mcxpurn.medium.jpg" alt="supervisor tree" /></a></div>

Looking at the OTP application supervision tree is a good way to understand the architecture of an OTP application.

The application supervisor `async_sup` starts up the following supervisors:

- `keyword_sup`. A `keyword_ser` process is created for every unique word in the StackExchange posts. This `keyword_ser` is linked to the `keyword_sup` supervisor (a `simple_one_for_one` supervisor). The `keyword_ser` child process maintains a list of document positions of a keyword  (an [inverted index](http://en.wikipedia.org/wiki/Inverted_index)).
- `facet_sup`. A `keyword_ser` process is also created for every unique facet category in the StackExchange posts. This `keyword_ser` process is linked to the `facet_sup` supervisor (a `simple_one_for_one` supervisor as well). The `keyword_ser` child process maintains a list of facet values with the IDs of the documents the facets appear in.

The application supervisor also start the following `gen_server` singleton processes:

- `stackoverflow_importer_ser`. This server imports the demo Stack Overflow data.
- `document_ser`. This server holds a copy of all documents, so it can return the original title and body of matching Stack Overflow posts in the results.
- `query_ser`. This server's task is to run the actual query and return results.
- `websocket_ser`. This server provides a HTTP frontend for the search engine.

No attention is given to fault tolerance (apart from the basic restart strategies), thus parts of the search index are lost if a `keyword_ser` process terminates.

<a name="data_import" ></a>
## Demo Data Import

The StackExchange data is provided as XML. Since some of the documents are quite large, it's not recommended to load the full XML documents in memory. The solution is to use a [SAX parser](http://nl.wikipedia.org/wiki/Simple_API_for_XML) which treats a XML file as a stream, and triggers events when new elements are discovered. The search server uses the excellent SAX parser from the [Erlsom](http://erlsom.sourceforge.net) library by Willem de Jong.

In the example below `erlsom:parse_sax` reads the XML file from `FilePath` and calls the function `sax_event` if an XML element is found.

<script src="https://gist.github.com/1203600.js?file=stackoverflow_importer_ser.erl"></script>

When the element is a `row` element (i.e. a post element), attributes like `Id`, `Title` and `Body` are stored in a dictionary. For every post a copy of all the attributes in `document_ser` is saved. This is used for returning the actual posts for a query match.  After that the `add_attribute_tokens` function is called:

<script src="https://gist.github.com/1203611.js?file=stackoverflow_import_ser.erl"></script>

The `add_attribute_tokens` function does two things. It calls `add_facet` (discussed later) and it creates a list of tuples with all the words and their position in the document. This process is called [tokenization](http://en.wikipedia.org/wiki/Tokenization). Each token/position tuple is then submitted to the `add_keyword_position` function of the `keyword_ser` for indexing.

<script src="https://gist.github.com/1205618.js?file=stackoverflow_import_ser.erl"></script>

<a name="indexing"></a>
## Indexing

Indexing of the tuples, or keywords, is handled by the `keyword_ser`. For every unique word a `keyword_ser` process is started if not already present. The state of a `keyword_ser` process is a dictionary with the document ID as key and a list of positions as value.  The document ID corresponds to the ID of the Stack Overflow post.

<script src="https://gist.github.com/1205643.js?file=keyword_ser.erl"></script>

The `keyword_server_name` function generates a unique name under which the `keyword_ser` process is registered, so the module can check if a keyword already has a process or a new process needs to be created.

<script src="https://gist.github.com/1205645.js?file=keyword_ser.erl"></script>

<a name="stemming" ></a>
## Stemming

[Stemming](http://en.wikipedia.org/wiki/Stemming) is the process for reducing inflected words to their base form. `Computing` and `computer` both are stemmed to `comput`. So when a user searches on `computing`, it also matches text that contains `computer`.  This makes it possible to return results that are relevant, but do not exactly match the query.

In our sample application all keywords are stemmed using the popular [Porter Algorithm](http://tartarus.org/~martin/PorterStemmer/). The [Erlang implementation](http://tartarus.org/~martin/PorterStemmer/porter.erl) by Alden Dima is used in the application.

<script src="https://gist.github.com/1205681.js?file=keyword_ser.erl"></script>

`erlang:phash2` is used to transform the stemmed name to a hash, to make sure the registered process name is valid.

<a name="faceting" >
## Faceting

[Faceted search](http://en.wikipedia.org/wiki/Faceted_search) is an important navigation feature for search engines. A user can drill down the search results by filtering on pre-defined attributes, like in this example of a digital camera search on CNET:

![Faceted search example](http://weblogs.asp.net/blogs/drnetjes/CNET_faceted_search.jpg)

As mentioned above, the data import the function `add_attribute_tokens` also calls the `add_facet` function. Using pattern matching the `Tags` and the `Creationdate` attributes are selected for faceting. `Tags` is a so called multivalue facet, as a Stack Overflow post can have one or more tags assigned. For every tag and creation date the `facet_ser:add_facet_value` function is called.

<script src="https://gist.github.com/1205699.js?file=stackoverflow_importer_ser.erl"></script>

`facet_ser` works very similar to `keyword_ser`. For every facet category, `Tag` or `Creationdate` in our case, a `facet_ser` processes is started. The state of a `facet_ser` is a dictionary with the `Tag` or `Creationdate` values as key and their document IDs as dictionary values.

<a name="query_and_ranking" ></a>
## Querying and Relevance Ranking

In previous sections is shown:

- how the XML demo data is parsed.
- how this data is stemmed and indexed by creating a `keyword_ser` process for every unique keyword.
- how this data is indexed for faceted search by creating a `facet_ser` process for every facet category.

With the function `stackoverflow_importer_ser:import()` these steps are executed, and your Erlang node is now ready for querying. So how does that work?

### Querying

Querying is handled by passing the user's query terms to the function `do_async_query` of the singleton `query_ser` server.  When calling this function you need to specify the module, function and optional reference attribute which will be called when query results are available.

<script src="https://gist.github.com/1205827.js?file=query_ser.erl"></script>

In the `handle_cast` the following steps are executed:

- `keyword_ser:do_query` return all document ids that contain one or more of the user's query terms, including the relevance ranking score, which will be discussed below.
-  All original documents are stored during indexing in a `document_ser` process. All matching documents are collected.
- The callback function is invoked with the matching documents and their ranking scores as arguments.
- Facet results are retrieved for any `FacetCategories` that are specified by calling `facet_ser:get_facets`.
- And the callback function is invoked a second time with the facet results as arguments.

<script src="https://gist.github.com/1205850.js?file=query_ser.erl"></script>

### Relevance Ranking

[Relevance](http://en.wikipedia.org/wiki/Relevance_\(information_retrieval\)) in this context denotes how well a retrieved document matches the user's search query. Most fulltext search-engines use the [BM25](http://en.wikipedia.org/wiki/Okapi_BM25) algorithm to determine the ranking score of each document, so let's use that too.

BM25 calculates a ranking score based on the query term frequency in each documents.

See the [async_bm25.erl](https://github.com/tty/classifieds_search/blob/master/src/async/src/async_bm25.erl) for the implementation.

<a name="display_search" ></a>
## Displaying the Search Results

As discussed, the `query_ser:do_async_query` can be called to query our full-text search engine. To allow users to send queries and see the result the `websocket_ser` module is created. This singleton `gen_server`starts up a [Misultin HTTP server](https://github.com/ostinelli/misultin) on Port 3000. If you browse to [http://localhost:3000](http://localhost:3000) you will see a search box. Communication with the search engine is done through websockets.

So, when a user posts a query, this message is received by the `websockets_ser:handle_websocket` receive block.  The `query_ser:do_async_query` function is called and query results are expected on `websockets_ser:query_results` function.

<script src="https://gist.github.com/1206002.js?file=websocket_ser.erl"></script>

The `query_results` function formats the results as HTML and sends this through the websocket. When received, the HTML is appended to the user's page.

<script src="https://gist.github.com/1206014.js?file=websocket_ser.erl"></script>

A similar process is executed when the facet results are received:

<script src="https://gist.github.com/1206022.js?file=websocket_ser.erl"></script>

<a name="improvements"></a>
## Improvements

Some obvious features that are lacking from this sample application:

- The author of this article is an Erlang newbie. Corrections/suggestions to the code are most welcome. You can send them to <ward@tty.nl>
- Pretty much no attention is given to performance / memory usage.
- Fault tolerence for the index data. When a server containing index state dies, it will not be revived.
- Tuple structures passed between modules are not specified. Would be nice to use record syntax for it.
- No unit/quickcheck/common test added.
- No function/type specifications.
- etc..

So, that why it's called a *sample* application ;-)
