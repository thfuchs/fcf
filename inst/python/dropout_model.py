from tensorflow.keras.models import clone_model
from tensorflow.keras.layers import Dropout

def dropout_model(model, dropout):
    """
    Create a keras function to predict with dropout
    Credits to https://github.com/keras-team/keras/issues/8826 and to 
    sfblake: https://medium.com/hal24k-techblog/how-to-generate-neural-network-
    confidence-intervals-with-keras-e4c0b78ebbdf
    
    model : keras model
    dropout : fraction dropout to apply to all layers
    
    Returns
    model_new : model with updated dropout rate
    """
    
    # 1. Use keras.models.clone_model
    model_new = clone_model(model)
    
    # 2. change dropout rate
    for layer in model_new.layers:
        if isinstance(layer, Dropout):
            layer.rate = dropout
    
    # 3. Compile the model
    # model_new.compile(optimizer="Adam", loss="mse")
    
    # 4. set_weights of cloned model with get_weights
    model_new.set_weights(model.get_weights())
    
    return model_new
