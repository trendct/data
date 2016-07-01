'''
  Replacer, kind of like a poor-man's agate-lookup.

'''
import pandas as pd

class Replacer:
    def __init__(self, url, in_col, out_col):
        self.df = pd.read_csv(url)
        self.in_col = in_col
        self.out_col = out_col
    

    def replace(self, term, error=False):
        term = term.upper()
        if term in self.df[self.in_col].unique():
            return self.df[self.df[self.in_col] == term][self.out_col].iloc[0]
        else:
            return error

    def replace_in_file(self, in_col, out_col, input_sheet,
                        output_sheet, error=False):
        in_df = pd.read_csv(input_sheet)
        in_df[out_col] = map (lambda x : self.replace(x) , in_df[in_col])
        in_df.to_csv(output_sheet)
        
