import numpy as np
import pandas as pd
import os
from scipy.io import loadmat
import random
import torch
from torch import nn
import itertools
import math

class GRUModel(nn.Module):
    def __init__(self,num_letters, num_layers, hidden_size):
        super(GRUModel, self).__init__()
        self.num_layers = num_layers
        #self.length = length
        self.hidden_size = hidden_size
        self.gru = nn.GRU(
            input_size=num_letters, 
            hidden_size=hidden_size, 
            num_layers=num_layers,
            batch_first=True
        )
        self.fc =  nn.Sequential(
                    nn.Linear(hidden_size,128),
                    nn.Dropout(0.5),
                    nn.ReLU(),
                    nn.Linear(128, 32),
                    nn.Dropout(0.5),
                    nn.ReLU(),
                    nn.Linear(32, 3)
                    )
        #self.fc = nn.Linear(hidden_size, 3)
        #self.hidden=None
    
    def forward(self, x):
        #hidden_state = self.init_hidden()
        output, self.hidden = self.gru(x)
        output = self.fc(output[:,-1,:])
        #output = self.drop(output)
        #output = self.fc2(output)        
        return output
    
class attention(nn.Module):
    def __init__(self,d_model,nhead,num_layers=3):
        super(attention, self).__init__()
        self.encoder_layer=nn.TransformerEncoderLayer(
                            d_model=d_model, 
                            nhead=nhead,
                            batch_first=True)
        self.encoder = nn.TransformerEncoder(self.encoder_layer, num_layers=num_layers)
        self.fc =  nn.Sequential(
                    nn.Linear(d_model,128),
                    nn.Dropout(0.5),
                    nn.ReLU(),
                    nn.Linear(128, 32),
                    nn.Dropout(0.5),
                    nn.ReLU(),
                    nn.Linear(32, 3)
                    ) 
        self.PE=PositionalEncoding(d_model)
    def forward(self, x):
        output= self.PE(x)
        output= self.encoder(x)
        output = self.fc(output[:,-1,:])
        return output
    
    
class PositionalEncoding(nn.Module): 
    '''reference https://zhuanlan.zhihu.com/p/107889011'''    
   #"Implement the PE function." 
    def __init__(self, d_model, dropout=0.1, max_len=500): 
      super(PositionalEncoding, self).__init__() 
      self.dropout = nn.Dropout(p=dropout) 
      pe = torch.zeros(max_len, d_model) 
      position = torch.arange(0, max_len).unsqueeze(1) 
      div_term = torch.exp(torch.arange(0, d_model, 2) * 
        -(math.log(10000.0) / d_model)) 
      pe[:, 0::2] = torch.sin(position * div_term) 
      if d_model%2 !=0:
          pe[:, 1::2] = torch.cos(position * div_term)[...,:-1]
      else:
          pe[:, 1::2] = torch.cos(position * div_term) 
      #pe[:, 1::2] = torch.cos(position * div_term) 
      pe = pe.unsqueeze(0) 
      pe.requires_grad_(False)
      self.register_buffer('pe', pe) 
    def forward(self, x): 
      #x = x + Variable(self.pe[:, :x.size(1)], requires_grad=False) 
      x = x + self.pe[:, :x.size(1)]
      return self.dropout(x)


def get_loader_length(loader):
    for i,_ in enumerate(loader):
        pass
    return i

def train(device, model, epochs, optimizer, loss_function, train_loader, valid_loader,stop=None):
    '''reference:https://pythonguides.com/pytorch-model-eval/#:~:text=PyTorch%20model%20eval%20train%20is%20defined%20as%20a,which%20act%20differently%20during%20training%20and%20evaluating%20time.'''
    # Early stopping
    #the_last_loss = 100
    #patience = 2
    #trigger_times = 0
    train_loss=[]
    val_loss=[]
    min_loss=100
    store=None
    batch_number=get_loader_length(train_loader)
    for epoch in range(1, epochs+1):
        
        for times, (inputs, labels) in enumerate(train_loader):
            model.train()
            inputs = inputs.to(device)
            labels = labels.to(device)
            # Zero the gradients
            optimizer.zero_grad()
            # Forward and backward propagation
            outputs = model(inputs.float())
            loss = loss_function(outputs, labels.long())
            loss.backward()
            optimizer.step()
            

            # Show progress
            if (times+1) % 5 == 0 or times == batch_number:
                train_loss.append(loss.item())
                the_current_loss = validation(model, device, valid_loader, loss_function)
                val_loss.append(the_current_loss)
                if val_loss[-1]<min_loss:
                    min_loss=val_loss[-1]
                    store=[epoch,times+1]
                print(
                    f"Epoch [{epoch }/{epochs}], "
                    f"Step [{times+1}/{batch_number+1}], "
                    f"Loss: {loss.item():.5f},"
                    f"The validation loss: {the_current_loss:.5f}"
                )
            if (epoch==epochs) & (times+1==stop):
                break
        '''
        # Early stopping
        the_current_loss = validation(model, device, valid_loader, loss_function)
        
        print(f"The validation loss: {the_current_loss:.5f}")
        '''
        '''
        if the_current_loss > the_last_loss:
            trigger_times += 1
            print('trigger times:', trigger_times)

            if trigger_times >= patience:
                print('Early stopping!\nStart to test process.')
                return model

        else:
            print('trigger times: 0')
            trigger_times = 0

        the_last_loss = the_current_loss
        '''
    print(store)   
    return model,train_loss,val_loss



def validation(model, device, valid_loader, loss_function):
    # Settings
    model.eval()
    loss_total = 0

    # Test validation data
    with torch.no_grad():
        inputs, labels = valid_loader
        inputs = inputs.to(device)
        labels = labels.to(device)
        outputs = model(inputs.float())
        loss = loss_function(outputs, labels.long())
    return loss
            
         
def test(device, model, test_loader,loss_function):
    # Settings
    model.eval()

    with torch.no_grad():
        inputs, labels = test_loader
        inputs = inputs.to(device)
        labels = labels.to(device)
        outputs = model(inputs.float())
        loss = loss_function(outputs, labels.long())
        m = nn.Softmax(dim=1)
        pred = m(outputs).max(1).indices 
        accuracy=sum(pred==labels)/len(labels)

    print(f"loss: {loss}, "
          f"accuracy: {accuracy} ")    
    
    return loss,accuracy


def get_iter(epoch,batch,stop,new_batch):
    total_iter=(epoch-1)*batch+stop
    print(f'new epoch: {total_iter//new_batch +1}')
    print(f'new_batch: {total_iter%new_batch+1}')
    
    
    