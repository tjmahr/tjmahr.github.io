module Jekyll
  module ConcatAuthors
    def concat_authors(input)
      if input.length > 1
        input[input.length - 1] = "& " + input.last
        input_string = input.join(", ")
      elsif input.length == 1
        input_string = input[0]
      end
      return input_string
    end
    
    def count_authors(input)
      return input.length
    end
    
    def doi_link(input)
      doi_link_string = "[" + input + "](http://doi.org/" + input + ")."
      return doi_link_string
    end
    
  end
end

Liquid::Template.register_filter(Jekyll::ConcatAuthors)

