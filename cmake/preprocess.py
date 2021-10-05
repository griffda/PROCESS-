import subprocess
import argparse


def arguments_to_python_compatible(sources, location, output, context):
    sources = sources.replace('\n','').strip()
    location = location.replace('\n','').strip()
    output = output.replace('\n','').strip()
    context = context.replace('\n','').strip()

    sources_list = sources.split(';')

    prepend = location + '/' if location[-1] != '/' else location
    sources_path_list = [prepend+i for i in sources_list]
    
    prepend = output + '/' if output[-1] != '/' else output
    output_path_list = [prepend+i for i in sources_list]

    context_list = [i.split('=') for i in context.split(';')]
    context_dict = {k: v for k, v in context_list} if context_list else {}

    return sources_path_list, output_path_list, context_dict


def is_string(x):
    temp = x.split('.')
    if x.isdigit() or (len(temp) == 2 and temp[0].isdigit and temp[1].isdigit()): # number
        return False
    return True


def preprocess_sources(sources, context, location, output):
    sources_path_list, output_path_list, context_dict = arguments_to_python_compatible(sources, context, location, output)

    context_string_list = []

    for k, v in context_dict.items():
        if is_string(v):
            context_string_list.append(f'-D{k}=\"\'{v}\'\"'.replace('\n',''))
        else:
            context_string_list.append(f'-D{k}=\"{v}\"'.replace('\n',''))

    context_string = ' '.join(context_string_list)

    for source, out in zip(sources_path_list, output_path_list):
        subprocess.call(f'gfortran -E -cpp {context_string} {source} > {out}', shell=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument('config_file',
                        help='absolute location of preprocess.txt configuration file',
                        type=str)

    arguments =  parser.parse_args()


    with open(f'{arguments.config_file}', 'r') as file:
        f = file.readlines()

    preprocess_sources(*f)
