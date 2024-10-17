import numpy as np
import pandas as pd
import os
from scipy.io import loadmat
import random
import torch
from torch import nn
import itertools
import copy
from myMice import Mydata
import myMice
import dl
from sklearn.model_selection import train_test_split
device = "cuda" if torch.cuda.is_available() else "cpu"
print(f"Using {device} device")

Dir='Data/Opp_Sex'
new_path=os.path.join(os.getcwd(), Dir)    
MD=Mydata(new_path)
MD.process() 
mydic=MD.get_data(method='origion')
'''
MD.b
for i in mydic.keys():
    print(i,mydic[i].shape)
'''

'''
mice 409:
'''
#key_train=['608034_409', '608102_412', '608102_414','608103_416']
key_train=['608034_409']
#key_test=['608103_417', '608103_418']
r=mydic[key_train[0]][:,-1]

train_set=myMice.sample(mydic,key_train,gap=90,lag=1)
#test_set=myMice.sample(mydic,key_test,gap=90,lag=0)
train,test=myMice.split_train_test(train_set)
test_x,test_y=myMice.get_test(test)
train_new, val=train_test_split(train_set, test_size=0.3, random_state=42)
data_loader=myMice.mice_loader(train_new,batch_size=64,random_state=82)

hidden_size = 64
learning_rate = 0.001
epochs=30
torch.manual_seed(10)
model = dl.GRUModel(num_letters=128,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         


Epoch [15/30], Step [35/68], Loss: 0.10049,The validation loss: 0.17429
val_error.index(min(val_error))


# early stopping
data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=15, batch=68, stop=35, new_batch=96)
11-28
hidden_size = 64
learning_rate = 0.001
epochs=11
torch.manual_seed(10)
model = dl.GRUModel(num_letters=128,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=28)         

torch.save(re, '409GRU.pt')
re = torch.load('409GRU.pt')

test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
loss: 0.08738557249307632, accuracy: 0.9614147543907166 

'''
mice 412:
'''
key_train=['608102_412']
train_set=myMice.sample(mydic,key_train,gap=90,lag=1)
#test_set=myMice.sample(mydic,key_test,gap=90,lag=0)
train,test=myMice.split_train_test(train_set)
test_x,test_y=myMice.get_test(test)
train_new, val=train_test_split(train_set, test_size=0.3, random_state=42)
data_loader=myMice.mice_loader(train_new,batch_size=64,random_state=82)

hidden_size = 64
learning_rate = 0.001
epochs=30
torch.manual_seed(10)
model = dl.GRUModel(num_letters=47,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         

Epoch [22/30], Step [15/68], Loss: 0.25391,The validation loss: 0.24000

val_error.index(min(val_error))

# early stopping
data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=22, batch=68, stop=15, new_batch=96)
16-4
hidden_size = 64
learning_rate = 0.001
epochs=16
torch.manual_seed(10)
model = dl.GRUModel(num_letters=47,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=4)  

torch.save(re, '412GRU.pt')
re = torch.load('412GRU.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
#loss: 0.29440319538116455, accuracy: 0.877734899520874
#loss: 0.2553710639476776, accuracy: 0.8918918967247009 
'''
mice 414:
'''
key_train=['608102_414']
train_set=myMice.sample(mydic,key_train,gap=90,lag=1)
#test_set=myMice.sample(mydic,key_test,gap=90,lag=0)
train,test=myMice.split_train_test(train_set)
test_x,test_y=myMice.get_test(test)
train_new, val=train_test_split(train_set, test_size=0.3, random_state=42)
data_loader=myMice.mice_loader(train_new,batch_size=64,random_state=82)

hidden_size = 64
learning_rate = 0.001
epochs=30
torch.manual_seed(10)
model = dl.GRUModel(num_letters=35,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         

Epoch [26/40], Step [40/68], Loss: 0.05000,The validation loss: 0.12637

val_error.index(min(val_error))

# early stopping
data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=26, batch=68, stop=40, new_batch=96)
19-13
hidden_size = 64
learning_rate = 0.001
epochs=19
torch.manual_seed(10)
model = dl.GRUModel(num_letters=35,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=13)  

torch.save(re, '414GRU.pt')
re = torch.load('414GRU.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
loss: 0.10341742634773254, accuracy: 0.9639871120452881 
loss: 0.07240838557481766, accuracy: 0.9749195575714111 

'''
mice 416:
'''
key_train=['608103_416']
train_set=myMice.sample(mydic,key_train,gap=90,lag=1)
#test_set=myMice.sample(mydic,key_test,gap=90,lag=0)
train,test=myMice.split_train_test(train_set)
test_x,test_y=myMice.get_test(test)
train_new, val=train_test_split(train_set, test_size=0.3, random_state=42)
data_loader=myMice.mice_loader(train_new,batch_size=64,random_state=82)

hidden_size = 64
learning_rate = 0.001
epochs=30
torch.manual_seed(10)
model = dl.GRUModel(num_letters=13,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         

Epoch [30/30], Step [60/68], Loss: 0.25122,The validation loss: 0.19743

val_error.index(min(val_error))

# early stopping
data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=30, batch=68, stop=60, new_batch=96)
22-17
hidden_size = 64
learning_rate = 0.001
epochs=22
torch.manual_seed(10)
model = dl.GRUModel(num_letters=13,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=17)  

torch.save(re, '416GRU.pt')
re = torch.load('416GRU.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
loss: 0.25302591919898987, accuracy: 0.8996784090995789 
loss: 0.2316630780696869, accuracy: 0.9061092734336853 
'''
mice 417:
'''
key_train=['608103_417']
train_set=myMice.sample(mydic,key_train,gap=90,lag=1)
#test_set=myMice.sample(mydic,key_test,gap=90,lag=0)
train,test=myMice.split_train_test(train_set)
test_x,test_y=myMice.get_test(test)
train_new, val=train_test_split(train_set, test_size=0.3, random_state=42)
data_loader=myMice.mice_loader(train_new,batch_size=64,random_state=82)

hidden_size = 64
learning_rate = 0.001
epochs=30
torch.manual_seed(10)
model = dl.GRUModel(num_letters=40,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         

Epoch [25/30], Step [65/74], Loss: 0.13101,The validation loss: 0.15502

val_error.index(min(val_error))

# early stopping
data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=25, batch=74, stop=65, new_batch=105)
17-18
18-57correct
hidden_size = 64
learning_rate = 0.001
epochs=18
torch.manual_seed(10)
model = dl.GRUModel(num_letters=40,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=57)  

torch.save(re, '417GRU.pt')
re = torch.load('417GRU.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
loss: 0.16373106837272644, accuracy: 0.9385342597961426
loss: 0.13405537605285645, accuracy: 0.945035457611084 

'''
mice 418:
'''
key_train=['608103_418']
train_set=myMice.sample(mydic,key_train,gap=90,lag=1)
#test_set=myMice.sample(mydic,key_test,gap=90,lag=0)
train,test=myMice.split_train_test(train_set)
test_x,test_y=myMice.get_test(test)
train_new, val=train_test_split(train_set, test_size=0.3, random_state=42)
data_loader=myMice.mice_loader(train_new,batch_size=64,random_state=82)

hidden_size = 64
learning_rate = 0.001
epochs=30
torch.manual_seed(10)
model = dl.GRUModel(num_letters=34,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         

Epoch [29/30], Step [30/68], Loss: 0.10646,The validation loss: 0.12751

val_error.index(min(val_error))

# early stopping
data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=29, batch=68, stop=30, new_batch=96)
21-15
hidden_size = 64
learning_rate = 0.001
epochs=21
torch.manual_seed(10)
model = dl.GRUModel(num_letters=34,num_layers=2, hidden_size=hidden_size).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=15)  

torch.save(re, '418GRU.pt')
re = torch.load('418GRU.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
loss: 0.11181993782520294, accuracy: 0.9569131731987 
loss: 0.10960549861192703, accuracy: 0.958842396736145 
#introduction
#EDA
#methods
#conclusion