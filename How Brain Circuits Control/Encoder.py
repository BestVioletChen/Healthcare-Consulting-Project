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
import math
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

train_set=myMice.sample(mydic,key_train,gap=90,lag=1)
#test_set=myMice.sample(mydic,key_test,gap=90,lag=0)
train,test=myMice.split_train_test(train_set)
test_x,test_y=myMice.get_test(test)
train_new, val=train_test_split(train_set, test_size=0.3, random_state=42)
data_loader=myMice.mice_loader(train_new,batch_size=64,random_state=82)


learning_rate = 0.001
epochs=50
torch.manual_seed(10)
model = dl.attention(d_model=128,nhead=8).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         


val_error.index(min(val_error))
Epoch [23/30], Step [35/68], Loss: 0.09936,The validation loss: 0.18293
#13
# early stopping

data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=23, batch=68, stop=35, new_batch=96)
16-92
learning_rate = 0.001
epochs=16
torch.manual_seed(10)
model = dl.attention(d_model=128,nhead=8).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=92)         

torch.save(re, '409former.pt')
re = torch.load('409former.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
#loss: 0.09511274844408035, accuracy: 0.9659163355827332 
loss: 0.12406297028064728, accuracy: 0.9556269645690918
loss: 0.06671130657196045, accuracy: 0.9762057662010193 
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

learning_rate = 0.001
epochs=50
torch.manual_seed(10)
model = dl.attention(d_model=47,nhead=47).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         


Epoch [19/30], Step [60/68], Loss: 0.14947,The validation loss: 0.28049

val_error.index(min(val_error))

# early stopping

data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=19, batch=68, stop=60, new_batch=96)
hidden_size = 64
learning_rate = 0.001
epochs=14
torch.manual_seed(10)
model = dl.attention(d_model=47,nhead=1).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=37)         

torch.save(re, '412former.pt')
re = torch.load('412former.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
#loss: 0.23413431644439697, accuracy: 0.916344940662384 
loss: 0.20313005149364471, accuracy: 0.9099099040031433 
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

learning_rate = 0.001
epochs=50
torch.manual_seed(10)
model = dl.attention(d_model=35,nhead=5).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         

Epoch [29/30], Step [65/68], Loss: 0.04852,The validation loss: 0.14075

val_error.index(min(val_error))

# early stopping
data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=29, batch=68, stop=65, new_batch=96)
21-50
learning_rate = 0.001
epochs=21
torch.manual_seed(10)
model = dl.attention(d_model=35,nhead=5).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=50)         

torch.save(re, '414former.pt')
re = torch.load('414former.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
#loss: 0.07843395322561264, accuracy: 0.9729903340339661 
#loss: 0.06655470281839371, accuracy: 0.9704179763793945 
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

learning_rate = 0.001
epochs=30
torch.manual_seed(10)
model = dl.attention(d_model=13,nhead=1).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         

#Epoch [28/30], Step [65/68], Loss: 0.28623,The validation loss: 0.26181
27/5  0.21284
min(val_error) 0.2428
test_error[val_error.index(min(val_error))]

# early stopping
data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=28, batch=68, stop=65, new_batch=96)
19-46
learning_rate = 0.001
epochs=27
torch.manual_seed(10)
model = dl.attention(d_model=13,nhead=1).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=5)         

torch.save(re, '416former.pt')
re = torch.load('416former.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
#loss: 0.3032018840312958, accuracy: 0.8816720247268677 
#loss: 0.28885141015052795, accuracy: 0.8816720247268677
loss: 0.2810989320278168, accuracy: 0.8881028890609741 
loss: 0.36770153045654297, accuracy: 0.8610932230949402 
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

learning_rate = 0.001
epochs=50
torch.manual_seed(10)
model = dl.attention(d_model=40,nhead=4).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         

Epoch [14/30], Step [20/74], Loss: 0.07149,The validation loss: 0.18168

val_error.index(min(val_error))

# early stopping
data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=14, batch=74, stop=20, new_batch=105)
10-38
learning_rate = 0.001
epochs=10
torch.manual_seed(10)
model = dl.attention(d_model=40,nhead=4).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=38)         

torch.save(re, '417former.pt')
re = torch.load('417former.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
#loss: 0.1558212786912918, accuracy: 0.9432623982429504 
#loss: 0.17469966411590576, accuracy: 0.9367612600326538 

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

learning_rate = 0.001
epochs=50
torch.manual_seed(10)
model = dl.attention(d_model=34,nhead=2).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader)         


Epoch [27/30], Step [55/68], Loss: 0.05264,The validation loss: 0.18204

val_error.index(min(val_error))

# early stopping
data_loader=myMice.mice_loader(train_set,batch_size=64,random_state=82)
dl.get_loader_length(data_loader)
dl.get_iter(epoch=27, batch=68, stop=55, new_batch=96)

learning_rate = 0.001
epochs=19
torch.manual_seed(10)
model = dl.attention(d_model=34,nhead=2).to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
loss_function = nn.CrossEntropyLoss()
train_loader=data_loader
valid_loader=[torch.tensor(val[:,:,:-1]),torch.tensor(val[:,-1,-1])]
re,test_error,val_error=dl.train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=96)         

torch.save(re, '418former.pt')
re = torch.load('418former.pt')
test_loader=[test_x,test_y]
loss,accuracy=dl.test(device, re, test_loader,loss_function)
#loss: 0.11532516032457352, accuracy: 0.958842396736145 
#loss: 0.1604926735162735, accuracy: 0.9305465817451477 


#introduction
#EDA
#methods
#conclusion

from torch.utils.tensorboard import SummaryWriter

# default `log_dir` is "runs" - we'll be more specific here
writer = SummaryWriter('runs/fashion_mnist_experiment_1')

