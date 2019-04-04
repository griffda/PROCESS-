import pandas as pd
raw_data = {
 #       'bt': [1], 
 #       'beta': [1.2],
        'rmajor': [1.3]}
df = pd.DataFrame(raw_data, columns = ['rmajor'])
#df.to_csv('plot_data.csv', encoding='utf-8', index=False)
df.to_csv('plot_data.csv', encoding='utf-8', mode='a', header=False, index=False)
