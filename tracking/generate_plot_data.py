import pandas as pd
tracking_list = ["bt", "beta", "rmajor"]
raw_data = {
        'bt': [1], 
        'beta': [1.2],
        'rmajor': [1.3]}
#df = pd.DataFrame(raw_data, columns = ['bt', 'beta', 'rmajor'])
df = pd.DataFrame(raw_data, columns = tracking_list)
#df.to_csv('plot_data.csv', encoding='utf-8', index=False)
df.to_csv('plot_data.csv', encoding='utf-8', mode='a', header=True, index=False)
print(df)
