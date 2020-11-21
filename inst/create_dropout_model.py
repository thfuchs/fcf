from keras.models import Model, Sequential
from keras import backend as K

def create_dropout_model(model, dropout):
    """
    Create a keras function to predict with dropout (Credits to sfblake)
    model : keras model
    dropout : fraction dropout to apply to all layers
    
    Returns
    predict_with_dropout : keras function for predicting with dropout
    """
    
    # Load the config of the original model
    conf = model.get_config()
    # Add the specified dropout to all layers
    for layer in conf['layers']:
        # Dropout layers
        if layer["class_name"]=="Dropout":
            layer["config"]["rate"] = dropout
        # Recurrent layers with dropout (and without recurrent dropout)
        elif "dropout" in layer["config"].keys():
            layer["config"]["dropout"] = dropout
            layer["config"]["recurrent_dropout"] = 0
            
    # Create a new model with specified dropout
    if type(model)==Sequential:
        # Sequential
        model_dropout = Sequential.from_config(conf)
    else:
        # Functional
        model_dropout = Model.from_config(conf)
    
    model_dropout.set_weights(model.get_weights())
    
    return model_dropout
