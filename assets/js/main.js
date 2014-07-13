$(function() {
  window.App = {
    init: function() {
      this.initializeIndex();
      this.populateIndex();
      this.setupSearchInput();
    },

    initializeIndex: function() {
      this.postIndex = lunr(function () {
        this.field('title', { boost: 10 });
        this.field('category');
        this.field('content');
        this.ref('href');
      });
    },

    populateIndex: function() {
      $.getJSON('/search.json', function (data) {
        data.pop(); // Last entry is always null

        window.App.searchEntries = data;

        $.each(data, function (index, value) {
          window.App.postIndex.add(value);
        });
      });
    },

    setupSearchInput: function() {
      $("#search").select2({
        allowClear: true,
        minimumInputLength: 3,
        query: this.performSearch
      })
      .on('select2-selecting', function (event) {
        window.location = event.choice.id;
      });
    },

    performSearch: function(query) {
      var results = window.App.postIndex.search(query.term);
      var data = { results: []};

      $.each(results, function(index, result) {
        var post = $.grep(window.App.searchEntries, function (entry) {
          return entry.href == result.ref;
        })[0];

        data.results.push({id: post.href, text: post.title });
      });

      query.callback(data);
    }
  };

  window.App.init();
});
