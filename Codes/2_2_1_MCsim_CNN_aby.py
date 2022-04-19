import numpy as np
import pandas as pd
pd.options.mode.chained_assignment = None  # default='warn'

from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score, mean_squared_error

from numpy.random import seed
from sklearn.preprocessing import StandardScaler

import time
import pyreadr

import torch
import torch.nn as nn
import torch.optim as optimizers
from torch.autograd import Variable

import math

#-- Load data
reg_raw_data = pyreadr.read_r('.Input/reg_raw_data.rds') # Please change the path
test_raw_data = pyreadr.read_r('.Input/test_raw_data.rds') # Please change the path

df = reg_raw_data[None]
df2 = test_raw_data[None]

#-- Define functions for simulation

# Dimension transformation for Torch
def DimTrans(A):
    B = np.zeros([A.shape[0],A.shape[3],A.shape[1],A.shape[2]])
    for i in range(A.shape[3]):
                   B[:,i,:,:] = A[:,:,:,i]
    return B

# Function that transforms the 'rate'(x3) into series data by averaging. 
def AvrTrans(A):
    A = A.cpu().numpy()
    B = np.empty(A.shape[0])
    for i in range(A.shape[0]):
        B[i] = np.mean(A[i,:,:,:]).reshape(-1)
        B = B.astype('float')
    return B

# Prediction function
def batch_pred(X):
    global x
    pred_list = []
    batch_size = 1
    x = 0
    for i in range(math.ceil(X.shape[0]/batch_size)):
        with torch.no_grad():
            temp_X = X[x:x+batch_size].to(device)
            
            # For independent multiple inputs
            x0 = temp_X[:,0,:,:].unsqueeze(1)
            x1 = temp_X[:,1,:,:].unsqueeze(1)
            x2 = temp_X[:,2,:,:].unsqueeze(1)
            x3 = temp_X[:,3,:,:].unsqueeze(1)
            
            x3 = AvrTrans(x3)
            x3 = torch.from_numpy(x3).float().view(-1,1)
            x3 = x3.to(device)
            
            pred = model(x0,x1,x2,x3).detach().cpu().numpy().astype('float').tolist()
        pred_list.extend(pred)
        x = x + batch_size
    return pred_list

#-- Model architecture
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

class CNN_LF(nn.Module):
    def __init__(self):
        super(CNN_LF, self).__init__()
        self.features0 = nn.Sequential(
            nn.Conv2d(1, 8, 3, 1, 1),
            nn.MaxPool2d(2),
            nn.ReLU(),
        )
        self.features1 = nn.Sequential(
            nn.Conv2d(1, 8, 3, 1, 1),
            nn.MaxPool2d(2),
            nn.ReLU(),
        )
        self.features2 = nn.Sequential(
            nn.Conv2d(1, 8, 3, 1, 1),
            nn.MaxPool2d(2),
            nn.ReLU(),
        )
        
        self.fc1 = nn.Sequential(
            nn.Linear(8*3*3,16),
            nn.ReLU(),
        )
        self.fc2 = nn.Linear(16,1)
        self.fc3 = nn.Sequential(
            nn.Linear(4,16),
            nn.ReLU(),
        )
        self.regressor = nn.Linear(16, 1)
        
    def forward(self, x0, x1, x2, x3):
        x0 = self.features0(x0)
        x1 = self.features1(x1)
        x2 = self.features2(x2)
        
        x0 = x0.view(x0.size(0), -1)
        x1 = x1.view(x1.size(0), -1)
        x2 = x2.view(x2.size(0), -1)
        
        x0 = self.fc1(x0)
        x1 = self.fc1(x1)
        x2 = self.fc1(x2)
        
        x0 = self.fc2(x0)
        x1 = self.fc2(x1)
        x2 = self.fc2(x2)
        
        x = torch.cat((x0,x1,x2,x3), dim=1)
        x = self.fc3(x)
        x = self.regressor(x)
        return x

#-- Run 1000 simulations
epoch_list =[]
rsqTrain_list = []
rsqVal_list = []
rsqTest_list = []
rmseTrain_list = []
rmseVal_list =[]
rmseTest_list =[]

simIDs = np.unique(df.sim)
iterMax = len(simIDs)
for simID in simIDs:
    
    print('Simulation N: {}/{}'.format(simID,iterMax))
    t = time.time()
    
    data = df[df.sim==simID]
    data2 = df2[df2.sim==simID]
    
    # Standardization prcess
    scaler_alpha = StandardScaler().fit(data[['alpha']].values)
    data.alpha = scaler_alpha.transform(data[['alpha']].values)
    data2.alpha = scaler_alpha.transform(data2[['alpha']].values)

    scaler_beta = StandardScaler().fit(data[['beta']].values)
    data.beta = scaler_beta.transform(data[['beta']].values)
    data2.beta = scaler_beta.transform(data2[['beta']].values)

    scaler_ymax = StandardScaler().fit(data[['ymax']].values)
    data.ymax = scaler_ymax.transform(data[['ymax']].values)
    data2.ymax = scaler_ymax.transform(data2[['ymax']].values)

    scaler_rate = StandardScaler().fit(data[['rate']].values)
    data.rate = scaler_rate.transform(data[['rate']].values)
    data2.rate = scaler_rate.transform(data2[['rate']].values)
    
    # Load training and validation data
    data.sort_values(by=['Y','X'], inplace=True)

    X = np.empty([0,6,6,4])
    Y  = []
    for i in set(data.subplot_id):
        for j in set(data.strip_id):
            alpha_temp = (data.alpha[(data.subplot_id==i) & (data.strip_id==j)])
            alpha_temp = np.array(alpha_temp).reshape(6,6)
            beta_temp = (data.beta[(data.subplot_id==i) & (data.strip_id==j)])
            beta_temp = np.array(beta_temp).reshape(6,6)
            ymax_temp = (data.ymax[(data.subplot_id==i) & (data.strip_id==j)])
            ymax_temp = np.array(ymax_temp).reshape(6,6)
            rate_temp = (data.rate[(data.subplot_id==i) & (data.strip_id==j)])
            rate_temp = np.array(rate_temp).reshape(6,6)
            X_array = np.stack([alpha_temp,beta_temp,ymax_temp,rate_temp],axis=2)
            X = np.append(X, X_array.reshape(1,6,6,4),axis=0)
            Y = np.append(Y, np.mean(data['yield'][(data.subplot_id==i) & (data.strip_id==j)]))

    # 30% is val, 70% is training
    seed(888)
    X_train, X_val = train_test_split(X, test_size=0.3, shuffle=True, random_state=888)
    Y_train, Y_val = train_test_split(Y, test_size=0.3, shuffle=True, random_state=888)

    ##################
    # Load test data #
    ##################
    data2.sort_values(by=['Y','X'], inplace=True)

    X_test = np.empty([0,6,6,4])
    Y_test  = []
    subplot_id = [] # for label 
    strip_id = [] # for label
    for i in set(data2.subplot_id):
        for j in set(data2.strip_id):
            subplot_id = np.append(subplot_id,i)
            strip_id = np.append(strip_id,j)
            
            alpha_temp = (data2.alpha[(data2.subplot_id==i) & (data2.strip_id==j)])
            alpha_temp = np.array(alpha_temp).reshape(6,6)
            beta_temp = (data2.beta[(data2.subplot_id==i) & (data2.strip_id==j)])
            beta_temp = np.array(beta_temp).reshape(6,6)
            ymax_temp = (data2.ymax[(data2.subplot_id==i) & (data2.strip_id==j)])
            ymax_temp = np.array(ymax_temp).reshape(6,6)
            rate_temp = (data2.rate[(data2.subplot_id==i) & (data2.strip_id==j)])
            rate_temp = np.array(rate_temp).reshape(6,6)
            X_array = np.stack([alpha_temp,beta_temp,ymax_temp,rate_temp],axis=2)
            X_test = np.append(X_test, X_array.reshape(1,6,6,4),axis=0)
            Y_test = np.append(Y_test, np.mean(data2['yield'][(data2.subplot_id==i) & (data2.strip_id==j)]))

    Y_train = np.array(Y_train)
    Y_val = np.array(Y_val)
    Y_test = np.array(Y_test)
    
    # Transform the dimension
    X_train = DimTrans(X_train)
    X_val = DimTrans(X_val)
    X_test = DimTrans(X_test)
    
    
    # Transform into tensor
    X_train = X_train.astype('float')
    X_val = X_val.astype('float')
    X_test = X_test.astype('float')

    X_train = torch.from_numpy(X_train).float()
    Y_train = torch.from_numpy(Y_train).float()
    X_val = torch.from_numpy(X_val).float()
    Y_val = torch.from_numpy(Y_val).float()
    X_test = torch.from_numpy(X_test).float()
    Y_test = torch.from_numpy(Y_test).float()

    Y_train = Y_train.view(-1,1)
    Y_val = Y_val.view(-1,1)
    Y_test = Y_test.view(-1,1)

    # Train process
    model = CNN_LF()
    model.to(device)
    
    counter = 0 # For earlystop

    PATIENCE = 10
    EPOCH = 500 
    BATCH_SIZE = 64

    losses = []
    torch.manual_seed(123)    # reproducible

    loss_func = nn.MSELoss()
    optimizer = optimizers.Adam(model.parameters(), lr=0.001)

    # Dataloader
    train_len = X_train.size()[0]
    val_len = X_val.size()[0]

    X = X_train
    Y = Y_train
    trainset = torch.utils.data.TensorDataset(X, Y)
    X = X_val
    Y = Y_val
    valset = torch.utils.data.TensorDataset(X, Y)

    train_loader = torch.utils.data.DataLoader(trainset, batch_size=BATCH_SIZE, shuffle=True)
    validation_loader = torch.utils.data.DataLoader(valset, batch_size=BATCH_SIZE, shuffle=True)
    data_loaders = {"train": train_loader, "val": validation_loader}
    data_lengths = {"train": train_len, "val": val_len}


    losses_train = []
    losses_val = []

    for epoch in range(EPOCH):

        # Each epoch has a training and validation phase
        for phase in ['train', 'val']:
            if phase == 'train':
                model.train(True)  # Set model to training mode
            else:
                model.train(False)  # Set model to evaluate mode

            running_loss = 0.0

            # Iterate over data.
            for data in data_loaders[phase]:

                # get the input images and their corresponding labels
                X, Y = data

                # flatten pts
                #key_pts = key_pts.view(key_pts.size(0), -1)

                # wrap them in a torch Variable
                X, Y = Variable(X), Variable(Y)

                # convert variables to floats for regression loss
                Y = Y.type(torch.cuda.FloatTensor)
                X = X.type(torch.cuda.FloatTensor)

                # For independent inputs
                x0 = X[:,0,:,:].unsqueeze(1)
                x1 = X[:,1,:,:].unsqueeze(1)
                x2 = X[:,2,:,:].unsqueeze(1)
                x3 = X[:,3,:,:].unsqueeze(1)

                x3 = AvrTrans(x3)
                x3 = torch.from_numpy(x3).float().view(-1,1)
                x3 = x3.to(device)

                # forward pass to get outputs
                outputs = model(x0,x1,x2,x3)

                # calculate the loss between predicted and target keypoints
                loss = loss_func(outputs, Y)

                # zero the parameter (weight) gradients
                optimizer.zero_grad()

                # backward + optimize only if in training phase
                if phase == 'train':
                    loss.backward()
                    # update the weights
                    optimizer.step()

                # print loss statistics
                running_loss += loss.data.item() #[0]

            epoch_loss = running_loss / data_lengths[phase]

            if phase == 'train':
                losses_train.append(epoch_loss)
            else:
                losses_val.append(epoch_loss)

                # Earlystopping
                if epoch == 0:
                    best_score = epoch_loss
                else:
                    best_score = min(losses_val)
                    if losses_val[-1] > best_score:
                        counter += 1
                    elif losses_val[-1] == best_score:
                        counter = 0
                        filepath = './model/CNN_LF_BestModel_' + str(simID) + '.pth'
                        torch.save(model.state_dict(), filepath) # save the best-score model

        # Earlystopping
        if counter == PATIENCE:
            print('---Training completed at the {}th epoch'.format(epoch+1))
            break
    
    # prediction
    model = CNN_LF()
    model.to(device)
    
    model.load_state_dict(torch.load(filepath))
    model.eval()
    
    pred_train = batch_pred(X_train)
    pred_val = batch_pred(X_val)
    pred_test = batch_pred(X_test)
    
    epoch_list.append(epoch+1-PATIENCE)
    rmseTrain_list.append(np.sqrt(mean_squared_error(Y_train.cpu().numpy(), pred_train)))
    rsqTrain_list.append(r2_score(Y_train.cpu().numpy(), pred_train))
    rmseVal_list.append(np.sqrt(mean_squared_error(Y_val.cpu().numpy(), pred_val)))
    rsqVal_list.append(r2_score(Y_val.cpu().numpy(), pred_val))
    rmseTest_list.append(np.sqrt(mean_squared_error(Y_test.cpu().numpy(), pred_test)))
    rsqTest_list.append(r2_score(Y_test.cpu().numpy(), pred_test))
    
    data2 = df2[df2.sim==simID]
    
    label = ['id','rate','pred','sim']
    plot = np.hstack([subplot_id.reshape(-1,1),strip_id.reshape(-1,1)])
    plot_list = []
    rate_list = []

    pred_list = []
    for i in range(X_test.shape[0]):
        for value in range(int(np.min(data2.rate)), int(np.max(data2.rate)+1)): 
            plot_list.append(str(int(plot[i,0])) + "_" + str(int(plot[i,1])))
            
            with torch.no_grad():
                temp_X = X_test[i].unsqueeze(0).to(device)

                x0 = temp_X[:,0,:,:].unsqueeze(1)
                x1 = temp_X[:,1,:,:].unsqueeze(1)
                x2 = temp_X[:,2,:,:].unsqueeze(1)

                x3 = np.array(value).astype('float')
                x3 = scaler_rate.transform([[x3]])
                x3 = torch.from_numpy(x3).float().view(-1,1)
                x3 = x3.to(device)

                pred = model(x0,x1,x2,x3).detach().cpu().numpy().astype('float').reshape(-1).tolist()
            pred_list.extend(pred)
            
        rate_list.extend(np.array(range(int(np.min(data2.rate)), int(np.max(data2.rate)+1))).tolist())

    sim_list = np.repeat(simID,len(plot_list))
    
    output = np.vstack([plot_list,rate_list,pred_list,sim_list])

    output = pd.DataFrame(output.T)
    output = output.set_axis(label, axis=1)
    output_path = './output/output_' + str(simID) + '.csv'
    output.to_csv(output_path)
    print('---Sensitivity analysis completed!')

    elapsed = time.time() - t
    print('---Elapsed time: {:.2f} min'.format(elapsed/60))
    
labelSummary = ['simID','best_epoch','rsqTrain','rsqVal','rsqTest','rmseTrain','rmseVal','rmseTest']
matSummary = np.vstack([simIDs,epoch_list,rsqTrain_list,rsqVal_list,rsqTest_list,rmseTrain_list,rmseVal_list,rmseTest_list])
matSummary = matSummary.T
matSummary = pd.DataFrame(matSummary)
matSummary.columns = labelSummary
matSummary.to_csv('./output/summary.csv')
