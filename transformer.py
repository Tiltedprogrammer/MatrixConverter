import os
import sys
from pathlib import Path
import re

def rreplace(s, old, new, occurrence):
    li = s.rsplit(old, occurrence)
    return new.join(li)

# straightforward for now, just find the top-most let expr
if __name__ == "__main__":
    args = sys.argv
    if (len(args) != 2):
        print ("Usage is: python transormer.py `path_to_hosc_file`")
    else:
        file = Path(args[1])    
        if(Path(file).exists()):
            #tranformation is the following: replace letrec with let, replace $ with `x` then bound unbounded variables and wrap with main
            with file.open() as f:
                f_lines = f.read()
                letrec = re.compile(r"\(letrec[\s\S]*\)")

                top_level_let = letrec.search(f_lines).group(0)[1:-1]
                top_level_let = top_level_let.replace("letrec","let")
                top_level_let = top_level_let.replace("$","x")

                data_types_fhw = re.findall(r"data .*;",f_lines)
                data_types_ghc = list(map (lambda d : f'{d.split(";")[0]} deriving Show', data_types_fhw))

                module_name = f"module {file.stem[0].capitalize()}{file.stem[1:]} where"
                
               

                top_level_let_fhw = rreplace(top_level_let,"in","   in",1)
                top_level_let_ghc = rreplace(top_level_let,"in","   in print $",1)
                
                fhw_file_string_main = f"main = {top_level_let_fhw}"
                ghc_file_string_main = f"main = {top_level_let_ghc}"

                data_types_fhw = '\n'.join(data_types_fhw)
                data_types_ghc = '\n'.join(data_types_ghc)

                fhw_file_string = '\n'.join([module_name,"\n",data_types_fhw,"\n",fhw_file_string_main])
                ghc_file_string = '\n'.join([module_name,"\n",data_types_ghc,"\n",ghc_file_string_main])
                # print(fhw_file_string)
                with open(f"{file.stem}FHW.hs","w") as fhw:
                    fhw.write(fhw_file_string)
                with open(f"{file.stem}GHC.hs","w") as ghc:
                    ghc.write(ghc_file_string)
                
        else:
            print("File {} not found".format(args[1]))    